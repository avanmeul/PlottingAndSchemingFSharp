module PlottingAndScheming.ThreeD

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Media3D
open _3DTools

//; 3d functions follow.
//
//(define abs-x-*-abs-y
//  (lambda (x y)
//    (* (abs x) (abs y))))
//
//(define log-2-*-x^2-+-y^2
//  (lambda (x y)
//    (log (+ (* 2 x x) (* y y)))))
//
//(define sin-x-*-cos-y
//  (lambda (x y)
//    (* (sin x) (cos y))))
//
//;(plot3d abs-x-*-abs-y
//;        -3 3 -3 3 21 21 '((-.6 .8 0.0) (-.5 -.4 .8) (.6 .5 .6)) 'half)
//
//;(make-3d-plot-object abs-x-*-abs-y -3 3 -3 3 21 21) 
//
//;(show-plot-object foo 'half () #t #t () #t () '((-.6 .8 0.0) (-.5 -.4 .8) (.6 .5 .6)))

let absXabsY x y = (abs x) * (abs y)
let sinXcosY x y = (sin x) * (cos y)

type mesh = {
    origin : (float * float * float);
    units : float;
    delta : float;
    rank : int; }

let simpleTriangle () =
    //let foo = Array3D.create 3 3 3  0.0
    let viewport = new Viewport3D ()
    let mesh = new MeshGeometry3D ()
    let rec iter x y =
        if y <= -3.0 then 
            ()
        else 
            let z = sinXcosY x y
            mesh.Positions.Add (new Point3D (x, y, z))
            let x, y = 
                let x = x - 0.1
                if x <= -3.0 then
                    3.0, (y - 0.1)
                else 
                    x, y
            iter x y
    //iter 3.0 3.0
    let point0 = new Point3D (0.0, 0.0, 0.0)
    let point1 = new Point3D (5.0, 0.0, 0.0)
    let point2 = new Point3D (0.0, 0.0, 5.0)
    mesh.Positions.Add point0
    mesh.Positions.Add point1
    mesh.Positions.Add point2
    mesh.TriangleIndices.Add 0
    mesh.TriangleIndices.Add 2
    mesh.TriangleIndices.Add 1
    let normal = new Vector3D (0.0, 1.0, 0.0)
    mesh.Normals.Add normal
    mesh.Normals.Add normal
    mesh.Normals.Add normal
    let material = new DiffuseMaterial (new SolidColorBrush (Colors.DarkKhaki))
    let geomod = new GeometryModel3D (mesh, material)
    let model = new ModelVisual3D ()
    model.Content <- geomod
    viewport.Children.Add model
    // Create another ModelVisual3D for light.
    let modvis = new ModelVisual3D ()
    modvis.Content <- new AmbientLight (Colors.White)
    viewport.Children.Add modvis
    // Create the camera.
    let cam = new PerspectiveCamera ()
    cam.FarPlaneDistance <- 100.0
    cam.LookDirection <- new Vector3D (-11.0, -10.0, -9.0)
    cam.UpDirection <- new Vector3D (0.0, 1.0, 0.0)
    cam.NearPlaneDistance <- 1.0
    cam.Position <- Point3D (11.0, 10.0, 9.0)
    cam.FieldOfView <- 70.0
    viewport.Camera <- cam
    let win = new Window ()
    win.SizeToContent <- SizeToContent.WidthAndHeight
    win.Content <- viewport
    win.Show ()

let CalculateNormal (p0 : Point3D) (p1 : Point3D) (p2 : Point3D) =
    let v0 = new Vector3D (p1.X - p0.X, p1.Y - p0.Y, p1.Z - p0.Z)
    let v1 = new Vector3D (p2.X - p1.X, p2.Y - p1.Y, p2.Z - p1.Z)
    Vector3D.CrossProduct (v0, v1)

let CreateTriangleModel p0 p1 p2 =
    let mesh = new MeshGeometry3D ()
    mesh.Positions.Add p0
    mesh.Positions.Add p1
    mesh.Positions.Add p2
    mesh.TriangleIndices.Add 0
    mesh.TriangleIndices.Add 1
    mesh.TriangleIndices.Add 2
    let normal = CalculateNormal p0 p1 p2
    mesh.Normals.Add normal
    mesh.Normals.Add normal
    mesh.Normals.Add normal
    let material  = new DiffuseMaterial (new SolidColorBrush (Colors.DarkKhaki))
    let model = new GeometryModel3D (mesh, material)
    let group = new Model3DGroup()
    group.Children.Add model
    group

let cube () =
    let cube = new Model3DGroup ()
    let p0 = new Point3D (0.0, 0.0, 0.0)
    let p1 =new Point3D (5.0, 0.0, 0.0)
    let p2 =new Point3D (5.0, 0.0, 5.0)
    let p3 =new Point3D (0.0, 0.0, 5.0)
    let p4 =new Point3D (0.0, 5.0, 0.0)
    let p5 =new Point3D (5.0, 5.0, 0.0)
    let p6 =new Point3D (5.0, 5.0, 5.0)
    let p7 = new Point3D (0.0, 5.0, 5.0)
    //front side triangles
    cube.Children.Add (CreateTriangleModel p3 p2 p6)
    cube.Children.Add (CreateTriangleModel p3 p6 p7) 
    //right side triangles
    cube.Children.Add (CreateTriangleModel p2 p1 p5) 
    cube.Children.Add (CreateTriangleModel p2 p5 p6) 
    //back side triangles
    cube.Children.Add (CreateTriangleModel p1 p0 p4)
    cube.Children.Add (CreateTriangleModel p1 p4 p5)
    //left side triangles
    cube.Children.Add (CreateTriangleModel p0 p3 p7)
    cube.Children.Add (CreateTriangleModel p0 p7 p4)
    //top side triangles
    cube.Children.Add (CreateTriangleModel p7 p6 p5)
    cube.Children.Add (CreateTriangleModel p7 p5 p4)
    //bottom side triangles
    cube.Children.Add (CreateTriangleModel p2 p3 p0)
    cube.Children.Add (CreateTriangleModel p2 p0 p1)    
    let model = new ModelVisual3D ()
    model.Content <- cube
    let viewport = new Viewport3D ()
    let material = new DiffuseMaterial (new SolidColorBrush (Colors.DarkKhaki))
    //let geomod = new GeometryModel3D (mesh, material)
    //let model = new ModelVisual3D ()
    //model.Content <- geomod
    viewport.Children.Add model
    // Create another ModelVisual3D for light.
    let modvis = new ModelVisual3D ()
    modvis.Content <- new AmbientLight (Colors.White)
    viewport.Children.Add modvis
    // Create the camera.
    let cam = new PerspectiveCamera ()
    cam.FarPlaneDistance <- 100.0
    cam.LookDirection <- new Vector3D (-11.0, -10.0, -9.0)
    cam.UpDirection <- new Vector3D (0.0, 1.0, 0.0)
    cam.NearPlaneDistance <- 1.0
    cam.Position <- Point3D (11.0, 10.0, 9.0)
    cam.FieldOfView <- 70.0
    viewport.Camera <- cam
    let win = new Window ()
    win.SizeToContent <- SizeToContent.WidthAndHeight
    win.Content <- viewport
    win.Show ()

let CreateRectangleFace p0 p1 p2 p3 surfaceColor (viewport : Viewport3D) =
    let mesh = new MeshGeometry3D ()
    mesh.Positions.Add p0
    mesh.Positions.Add p1
    mesh.Positions.Add p2
    mesh.Positions.Add p3
    mesh.TriangleIndices.Add 0
    mesh.TriangleIndices.Add 1
    mesh.TriangleIndices.Add 2
    mesh.TriangleIndices.Add 2
    mesh.TriangleIndices.Add 3
    mesh.TriangleIndices.Add 0
    let brush = new SolidColorBrush ()
    brush.Color <- surfaceColor
    let material = new DiffuseMaterial (brush)
    let geometry = new GeometryModel3D (mesh, material)
    let model = new ModelVisual3D ()
    model.Content <- geometry
    viewport.Children.Add (model)

let CreateWireFrame p0 p1 p2 p3 lineColor (viewport : Viewport3D) =
    let ssl = new ScreenSpaceLines3D ()
    ssl.Points.Add p0
    ssl.Points.Add p1
    ssl.Points.Add p1
    ssl.Points.Add p2
    ssl.Points.Add p2
    ssl.Points.Add p3
    ssl.Points.Add p3
    ssl.Points.Add p0
    ssl.Color <- lineColor
    ssl.Thickness <- 2.0
    viewport.Children.Add ssl

let GetNormalize (pt :  Point3D) xmin xmax ymin ymax zmin zmax =     
    let x = -1.0 + 2.0 * (pt.X - xmin) / (xmax - xmin)
    let y = -1.0 + 2.0 * (pt.Y - ymin) / (ymax - ymin)
    let z = -1.0 + 2.0 * (pt.Z - zmin) / (zmax - zmin)
    new Point3D (x, y, z)

type SimpleSurface = {
    mutable xmin : float;
    mutable xmax : float;
    mutable ymin : float;
    mutable ymax : float;
    mutable zmin : float;
    mutable zmax : float;
    mutable nx : int;
    mutable nz : int;
    mutable lineColor : Color;
    mutable surfaceColor : Color;
    mutable center : Point3D;
    mutable isHiddenLine : bool;
    mutable isWireframe : bool;
    mutable viewport3d : Viewport3D; } with
    static member create viewport = {
        xmin = -3.0;
        xmax = 3.0;
        ymin = -8.0;
        ymax = 8.0;
        zmin = -3.0;
        zmax = 3.0;
        nx = 30;
        nz = 30;
        lineColor = Colors.Black;
        surfaceColor = Colors.White;
        center = new Point3D ()
        isHiddenLine = false;
        isWireframe = true;
        viewport3d = viewport; }
    member x.CreateSurface f =
        let xmin, xmax, ymin, ymax, zmin, zmax, nx, nz, center = 
            x.xmin, x.xmax, x.ymin, x.ymax, x.zmin, x.zmax, x.nx, x.nz, x.center
        let dx = (xmax - xmin) / (float nx)
        let dz = (zmax - zmin) / (float nz)
        if not (nx < 2 || nz < 2) then
            let zero = new Point3D (0.0, 0.0, 0.0)
            let pts : Point3D[,] = Array2D.create nx nz zero
            for i in 0 .. 1 .. nx - 1 do
                let x = xmin + (float i) * dx
                for j in 0 .. 1 .. nz - 1 do
                    let z = zmin + (float j) * dz
                    let vec : Vector3D = new Vector3D (center.X, center.Y, center.Z)
                    let pt : Point3D = (f x z)
                    let ptVec = new Point3D (pt.X, pt.Y, pt.Z)
                    let res = ptVec + vec
                    let res = new Point3D (res.X, res.Y, res.Z)
                    pts.[i, j] <- res
                    let temp = GetNormalize pts.[i, j] xmin xmax ymin ymax zmin zmax
                    pts.[i, j] <- temp                
            let p : Point3D[] = Array.create 4 zero
            for i in 0 .. 1 .. nx - 2 do
                for j in 0 .. 1 .. nz - 2 do
                    p.[0] <- pts.[i, j]
                    p.[1] <- pts.[i, j + 1]
                    p.[2] <- pts.[i + 1, j + 1]
                    p.[3] <- pts.[i + 1, j]
                    //create rectangular face
                    if not x.isHiddenLine then
                        CreateRectangleFace p.[0] p.[1] p.[2] p.[3] x.surfaceColor x.viewport3d
                    //create wireframe
                    if x.isWireframe then
                        CreateWireFrame p.[0] p.[1] p.[2] p.[3] x.lineColor x.viewport3d

let xamlViewport () =
    let model = new Model3DGroup ()
    let model = new ModelVisual3D ()
//    model.Content <- cube
    let viewport = new Viewport3D ()
    let material = new DiffuseMaterial (new SolidColorBrush (Colors.DarkKhaki))
    //let geomod = new GeometryModel3D (mesh, material)
    //let model = new ModelVisual3D ()
    //model.Content <- geomod
    viewport.Children.Add model
    // Create another ModelVisual3D for light.
    let modvis = new ModelVisual3D ()
    modvis.Content <- new AmbientLight (Colors.White)
    viewport.Children.Add modvis
    // Create the camera.
    let cam = new PerspectiveCamera ()
    cam.FarPlaneDistance <- 100.0
    cam.LookDirection <- new Vector3D (-3.0, -3.0, -2.0)
    cam.UpDirection <- new Vector3D (0.0, 1.0, 0.0)
    cam.NearPlaneDistance <- 1.0
    cam.Position <- Point3D (3.0, 3.0, 2.0)
    cam.FieldOfView <- 70.0
    viewport.Camera <- cam
    viewport
    
type SimpleSurfaceTest = {
    window : Window;
    viewport : Viewport3D;
    surface : SimpleSurface; } with
    static member create viewport = 
        let Sinc x z =
            let r = (Math.Sqrt (x * x + z * z)) + 0.00001
            let y = (Math.Sin r) / r
            new Point3D (x, y, z)
        let AddSinc (surf : SimpleSurface) = 
            surf.xmin <- -8.0
            surf.xmax <- 8.0
            surf.zmin <- -8.0
            surf.zmax <- 8.0
            surf.ymin <- -1.0
            surf.ymax <- 1.0
            surf.CreateSurface Sinc
        let surf = SimpleSurface.create viewport
        surf.isHiddenLine <- false
        surf.viewport3d <- viewport
        AddSinc surf
        let win = new Window ()
        win.Content <- viewport
        {   window = win;
            viewport = viewport;
            surface = surf; }

let plot1 () =
    let viewport = xamlViewport ()
    let surfTest = SimpleSurfaceTest.create viewport
    let win = surfTest.window
    //win.SizeToContent <- SizeToContent.WidthAndHeight
    win.Show ()

let plot3d = (fun _ -> plot1 ())

//let threeDTabItem () =
//    let tabItem = new TabItem()
//    tabItem.Name <- "tab3d"
//    tabItem.Header <- "3d"
//    let pnl = new StackPanel ()
//    let btn = new Button ()
//    btn.Height <- 25.0
//    btn.Content <- "Plot"
//    btn.Click.Add (
//        fun _ ->
//            //cube ()
//            plot1 ()
//            )
//    pnl.Children.Add btn |> ignore
//    tabItem.Content <- pnl
//    tabItem