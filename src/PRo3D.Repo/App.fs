namespace DiscoverOpcs

open System
open System.IO

open Aardvark.UI
open Aardvark.UI.Primitives
  
open Aardvark.Base
open FSharp.Data.Adaptive
open DiscoverOpcs.Model
open Aardvark.SceneGraph.Opc
open PRo3D.Base
open Chiron


module App =
    let inline (==>) a b = Aardvark.UI.Attributes.attribute a b


    module Dialogs = 
        let onChooseFiles (chosen : list<string> -> 'msg) =
            let cb xs =
                match xs with
                | [] -> chosen []
                | x::[] when x <> null -> x |> Aardvark.Service.Pickler.json.UnPickleOfString |> List.map Aardvark.Service.PathUtils.ofUnixStyle |> chosen
                | _ -> chosen []//failwithf "onChooseFiles: %A" xs
            onEvent "onchoose" [] cb   
    
    //let importFolders (paths : list<string>) : list<OpcFolder> = 
    //  paths
    //    |> List.map(fun x ->
    //      if x |> Discover.isOpcFolder then x |> Opc
    //      elif x |> Discover.isSurface then x |> Surface
    //      elif x |> Discover.isSurfaceFolder then x |> SurfaceFolder
    //      else x |> Other
    //    )    
    
    let tryFileExists path = 
        if File.Exists path then Some path else None
    
    let tryDirectoryExists path = 
        if Directory.Exists path then Some path else None


    let scaleBoxes (boxes:List<Box3d>) (w:float) (h:float) = 
        let mutable min = boxes.Head.Min.XY
        let mutable max = boxes.Head.Max.XY

        let boxes = boxes |> List.map(fun x -> 
            let min = x.Min
            let max = x.Max

            if max.Y < min.Y then
                Box3d(V3d(min.X,max.Y,min.Z),V3d(max.X,min.Y,max.Z))
            else
                Box3d(min,max)
        )

        for box in boxes do
            if box.X.Min < min.X then
                min.X <- box.X.Min
            if box.X.Max > max.X then
                max.X <- box.X.Max
        
            if box.Y.Min < min.Y then
                min.Y <- box.Y.Min
            if box.Y.Max > max.Y then
                max.Y <- box.Y.Max
                

        // Moving all boxes into the first quadrant
        let positive_boxes = boxes |> List.map(fun box -> 
            Box2d(box.Min.XY - min,box.Max.XY - min)
        )

        printfn "positive_boxes %A" positive_boxes
        
        let range = max-min

        let unified_boxes = positive_boxes |> List.map( fun box -> 
            Box2d( (box.Min / range) , (box.Max / range) )
        )

        let scaled_boxes = unified_boxes |> List.map( fun box ->
            let min = V2d(box.Min.X * w,box.Min.Y *h)
            let max = V2d(box.Max.X * w,box.Max.Y *h)
            Box2d(min ,max)
        
        )

        scaled_boxes
                
            
    

    let update (model : Model) (msg : Message) =
        match msg with
        | SetPaths paths -> 
            let selectedPaths = paths |> List.choose tryDirectoryExists
            
            Log.startTimed "Discovering Opcs"
            
            let opcs = 
                selectedPaths 
                |> List.map Discover.superDiscovery
                |> HashMap.ofList
            
            let surfacePaths = 
                selectedPaths
                |> List.map Discover.superDiscoveryMultipleSurfaceFolder
                |> List.concat
            
            let box_info = 
                surfacePaths 
                |> List.map(fun dir -> 

                    let phDirs = Directory.GetDirectories(dir) |> Array.head |> Array.singleton
            
                    let patchHierarchies = [ 
                        for h in phDirs do
                        yield PatchHierarchy.load Serialization.binarySerializer.Pickle Serialization.binarySerializer.UnPickle (h |> OpcPaths)
                    ]


                    let info = 
                        patchHierarchies 
                        |> List.map(fun x -> x.tree |> QTree.getRoot) 
                        |> List.map(fun x -> x.info)


                    let box = 
                        patchHierarchies 
                        |> List.map(fun x -> x.tree |> QTree.getRoot) 
                        |> List.map(fun x -> x.info.GlobalBoundingBox)
                        |> List.fold (fun a b -> Box.Union(a, b)) Box3d.Invalid

                    (info,box)
                )

            let bboxes = 
                box_info
                |> List.map(fun (info,box) -> //transforming box to lon lat 
                    let min = CooTransformation.getLatLonAlt box.Min Planet.Mars
                    let max = CooTransformation.getLatLonAlt box.Max Planet.Mars
                    
                    Box3d(V3d(min.latitude, min.longitude, min.altitude), V3d(max.latitude, max.longitude, max.altitude))                    
                )
            let opc_surfaces =  (List.zip bboxes surfacePaths) |> List.map(fun (box,path) -> 
                {
                    filename =  List.last (String.split '\\' path)
                    path = path
                    bounds = box
                })
            
            let bboxes = 
                if bboxes.Length > 0 then 
                    scaleBoxes bboxes 400.0 400.0
                else
                    List.empty

            let bboxes = bboxes |> List.map(fun box -> 
                Box2d(box.Min + V2d(5.0,5.0),box.Max + V2d(5.0,5.0))
            )
            
            Log.stop()


            
            { model with 
                selectedPaths = selectedPaths |> IndexList.ofList
                opcPaths = opcs
                surfaceFolders = surfacePaths
                surfaces = opc_surfaces
                bboxes = bboxes
            }
        | Discover -> failwith ""
        | Enter i -> 
            { model with
                hover = i    
            }
        | Select i -> 
            let selected = 

                let s = model.surfaceFolders.[i]

                if model.highlightedFolders.Contains(s) then
                    model.highlightedFolders.Remove(s)
                else
                    model.highlightedFolders.Add(s)
            

            
            { model with
                highlightedFolders = selected
                selectedSurface = model.surfaces.[i]
            }
        | Save -> 
            let content = model |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty
            
            let dir = Directory.GetCurrentDirectory()
            let path = dir + "\\sav.json"

            File.WriteAllText(path, content)

            model 
        |Restore ->
            let dir = Directory.GetCurrentDirectory()
            let path = dir + "\\sav.json"

            if File.Exists(path) then
                let file = File.readAllText path
                let model:Model = file |> Json.parse |> Json.deserialize
                model
            else
                model
        
        | UpdateConfig cfg ->
            { model with dockConfig = cfg}
        | SurfacePropertiesMessage a ->
            let opcSurface = SurfacePropertiesApp.update model.selectedSurface a

            { model with selectedSurface = opcSurface }
    
    let viewPaths (model:AdaptiveModel) = 
    
        Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
            alist {
                for p in model.selectedPaths do
                    yield div [clazz "ui inverted item"][              
                        div [clazz "ui content"] [
                            div [clazz "ui header tiny"] [p |> text]
                        ]
                    ]
            }
        )
    
    let viewOpcPaths (model:AdaptiveModel) = 
        Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
            alist {
                for (folder,opclist) in model.opcPaths |> AMap.toASet |> ASet.toAList do
                    //yield Html.SemUi.accordion "blub" "boxes" true [
                    yield h3 [][text (Path.GetFileName folder)]
                    for opc in opclist do
                        yield div [clazz "ui inverted item"][
                            i [clazz "ui middle aligned box icon"] []
                            div [clazz "ui content"] [
                              div [clazz "ui header tiny"] [text opc]      
                            ]
                        ]
                  //]
            }
        )
    
    //let properties (model:AdaptiveModel) = 
    //    Incremental.div ([clazz "ui list"] |> AttributeMap.ofList) (
    //        alist {
    //            let! p = model.properties
    //            yield div [clazz "ui inverted item"][
    //                h3[clazz "ui"][text p.filename]
    //                h3[clazz "ui"][text p.path]
    //                h3[clazz "ui"][text p.bounds]
    //            ]
    //        }
    //    )

    let viewSurfacePaths (model:AdaptiveModel) = 
    
        Incremental.div ([clazz "ui very compact stackable inverted relaxed divided list"] |> AttributeMap.ofList) (
            alist {
                let! test = model.surfaceFolders
                let! hover = model.hover
                let! selected = model.highlightedFolders.Content

                for i in 0..test.Length-1 do
                    
                    let opc = test.[i]
                    let opc_name = List.last (String.split '\\' opc);
                   
                   
                    if selected.Contains opc then
                        yield h3 [style "color: red"; onMouseOver (fun _ -> Enter i) ;onMouseDoubleClick(fun _ -> UpdateProperties i); onClick (fun _ -> Select i)] [text (opc_name)]
                    else
                        if hover = i then
                            yield h3 [style "color: blue"; onMouseOver (fun _ -> Enter i);onMouseDoubleClick(fun _ -> UpdateProperties i); onClick (fun _ -> Select i)] [text (opc_name)]
                        else
                            yield h3 [style "color: white";onMouseOver (fun _ -> Enter i);onMouseDoubleClick(fun _ -> UpdateProperties i); onClick (fun _ -> Select i)] [text (opc_name)]
   
            }
        )
    
    let jsImportOPCDialog =
          "top.aardvark.dialog.showOpenDialog({tile: 'Select directory to discover OPCs and import', filters: [{ name: 'OPC (directories)'}], properties: ['openDirectory', 'multiSelections']}).then(result => {top.aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"
    
    let view (model : AdaptiveModel) =
    
        let myCss = [
            { kind = Stylesheet;  name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
            { kind = Stylesheet;  name = "semui-overrides"; url = "semui-overrides.css" }
            { kind = Script;      name = "semui";           url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
        ]
        
        let drawBox (box : Box2d) attributes =      
            Svg.rect <| attributes @ [
                "x" ==> sprintf "%f"      box.Min.X
                "y" ==> sprintf "%f"      box.Min.Y
                "width" ==> sprintf  "%f" box.SizeX
                "height" ==> sprintf "%f" box.SizeY            
            ]

        let viewPolygon =
            alist {
                let! test = model.surfaceFolders
                let! boxes = model.bboxes 
                let! hover = model.hover
                let! selected = model.highlightedFolders.Content

                for i in 0..boxes.Length-1 do
                
                    let opc = test.[i]

                    if selected.Contains opc then
                        yield drawBox boxes.[i] [style "stroke: red;   stroke-width:2;fill-opacity: .25;";onMouseOver (fun _ -> Enter i); onClick (fun _ -> Select i)]
                    else
                        if hover = i then
                            yield drawBox boxes.[i] [style "stroke: blue;   stroke-width:2;fill-opacity: .25;";onMouseOver (fun _ -> Enter i); onClick (fun _ -> Select i)]
                        else
                            yield drawBox boxes.[i] [style "stroke: white;  stroke-width:2;fill-opacity: .25;";onMouseOver (fun _ -> Enter i); onClick (fun _ -> Select i)] 
            }
    
        let svg =
            // read about svg elements here: https://www.w3schools.com/html/html5_svg.asp
            let attributes = 
                AttributeMap.ofList [
                    attribute "width" "420"; attribute "height" "420" 

                    // our event handlers require to search for the svg element to compute the coordinates realtive to.
                    // currently we use hard coded class name 'svgRoot' in our javascript code. see: aardvark.js
                    clazz "svgRoot"; 
                
                    // show a border for our svg
                    style "border: 5px solid white;"
                ]

            // finally create our svg. since our content is dynamic we use the incremental version of svg
            Incremental.Svg.svg attributes <| 
                alist {
                    yield! viewPolygon 
                }

        let jsImportOPCDialog =
            "top.aardvark.dialog.showOpenDialog({tile: 'Select directory to discover OPCs and import', filters: [{ name: 'OPC (directories)'}], properties: ['openDirectory', 'multiSelections']}).then(result => {top.aardvark.processEvent('__ID__', 'onchoose', result.filePaths);});"
        

        let importSurface =
            [
                text "Surfaces"
                i [clazz "dropdown icon"][] 
                div [ clazz "menu"] [
                    div [ clazz "ui inverted item";
                        Dialogs.onChooseFiles SetPaths;
                        clientEvent "onclick" (jsImportOPCDialog) 
                    ][
                        text "Import OPCs"
                    ]
                ]

            ]
        let topMenuItems (m:AdaptiveModel) = [ 
                
            //Navigation.UI.viewNavigationModes m.navigation  |> UI.map NavigationMessage 
            Html.Layout.horizontal [
                div [ clazz "ui"][
                    button[
                      onClick(fun _ -> Save)
                    ][text "Save"]
                ]
            ]

            Html.Layout.horizontal [
                div [ clazz "ui"][
                    button[
                      //onClick(fun _ -> Save)
                    ][text "show preview"]
                    button[
                      //onClick(fun _ -> Save)
                    ][text "add to scene"]
                    button[
                      //onClick(fun _ -> Save)
                    ][text "rm from scene"]
                ]
            ]
            Html.Layout.horizontal [
                div [ clazz "ui"][
                    button[
                      //onClick(fun _ -> Save)
                    ][text "export"]
                    button[
                      //onClick(fun _ -> Save)
                    ][text "launch Pro3d"]
                ]
            ]
                  
            //Html.Layout.horizontal [
            //    Html.Layout.boxH [ i [clazz "large Globe icon"][] ]
            //    Html.Layout.boxH [ Html.SemUi.dropDown m.scene.referenceSystem.planet ReferenceSystemAction.SetPlanet ] |> UI.map ReferenceSystemMessage
            //] 
            //Html.Layout.horizontal [
            //    scenepath m
            //]        
        ]        

        let menu (m : AdaptiveModel) =             

            div [clazz "menu-bar"] [
                // menu
                div [ clazz "ui top menu"; style "z-index: 10000; padding:0px; margin:0px"] [
                    onBoot "$('#__ID__').dropdown('on', 'hover');" (
                        div [ clazz "ui dropdown item"; style "padding:0px 5px"] [
                            i [clazz "large sidebar icon"; style "margin:0px 2px"] []
                            
                            div [ clazz "ui menu"] [
                                //import surfaces
                                //div [ clazz "ui dropdown item"; style "width: 150px"] importSurface
                                //scene menu
                                div [ clazz "button"] [
                                    div [ clazz "ui inverted item";
                                        Dialogs.onChooseFiles SetPaths;
                                        clientEvent "onclick" (jsImportOPCDialog) 
                                    ][
                                        text "add surface"
                                    ]
                                ]
                                div [ clazz "button"] [
                                    div [ clazz "ui inverted item";
                                        //Dialogs.onChooseFiles SetPaths;
                                        //clientEvent "onclick" (jsImportOPCDialog) 
                                    ][
                                        text "add anno"
                                    ]
                                ]
                            ] 
                        ]
                    )
                ]
            ]
        page (fun request -> 
            match Map.tryFind "page" request.queryParams with
                | Some "files" -> 
                    require Html.semui (
                          body [style"width: 40%; height:100%; background: transparent; overflow: auto"; ] [
                              div [clazz "ui inverted segment"] [
                                  h1 [clazz "ui"][text "Discovered Surface"]
                                  br []
                                  viewSurfacePaths model
                              ]
                          ]
                    )
                | Some "properties" ->
                    require Html.semui (
                        body [style"width: 100%; height:100%; background: transparent; overflow: auto"; ] [
                            //div [clazz "ui inverted segment"] [
                            //    h1 [clazz "ui"][text "Properties"]
                            //    properties model
                            //    button[onClick(fun _ -> OpenExplorer) ][text "Open Folder"]
                            //]
                            SurfacePropertiesApp.view model.selectedSurface |> UI.map SurfacePropertiesMessage
                        ]
                    )
                | Some "boxes" ->
                    body[style "width: 100%; height:100%; background: transparent; overflow: auto";] [
                        svg // here comes the actual svg
                    ]
                | Some "scene" ->
                    require Html.semui (
                        body [style"width: 100%; height:100%; background: transparent; overflow: auto"; ] [
                          h1 [clazz "ui"][text "Scene"]
                        ]
                    )
                | Some "3d_preview" ->
                    require Html.semui (
                        body [style"width: 100%; height:100%; background: transparent; overflow: auto"; ] [
                          h1 [clazz "ui"][text "3D Preview"]
                        ]
                    )
                | Some other ->
                    let msg = sprintf "Unknown page: %A" other
                    body [] [
                        div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
                    ]  

                | None -> 
                    require(myCss) (
                        body [][   
                            div[clazz "ui menu"; style "padding:0; margin:0; border:0"] [
                                yield (menu model)
                                for t in (topMenuItems model) do
                                    yield div [clazz "item topmenu"] [t]
                            ]
                            div[clazz "dockingMainDings"] [
                                model.dockConfig
                                |> docking [                                           
                                    style "width:100%; height:100%; background:#F00"
                                    onLayoutChanged UpdateConfig ]
                            ]
                        ]
                    )

            )
    
    
    let threads (model : Model) = 
        ThreadPool.empty
        
    let initPaths = [] // @"G:\New_3D_Data\New_MSL_Data_jan_2018" |> List.singleton
    
    let opcPaths = 
        initPaths      
        |> List.map DiscoverOpcs.Discover.discoverOpcs 
        |> List.concat
    
    let initial =  { 
        selectedPaths = initPaths |> IndexList.ofList
        opcPaths = HashMap.empty //opcPaths |> IndexList.ofList
        selectedSurface = {
            filename = ""
            path = ""
            bounds = Box3d()
        }
        surfaces = List.empty
        surfaceFolders = List.empty
        bboxes = List.empty
        hover = -1
        highlightedFolders = HashSet.empty
        dockConfig = Model.ui.dockConfig
    }
    

    let app =
        let initial = Restore |> update initial
        {
            unpersist = Unpersist.instance     
            threads = threads 
            initial = initial.selectedPaths.AsList |> SetPaths |> update initial
            update = update 
            view = view
        }
