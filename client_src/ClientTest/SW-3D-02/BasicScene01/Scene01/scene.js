function initializeScene(engine){
    // Get the Canvas element from our HTML below
    var scene = new BABYLON.Scene(engine);
    var light = new BABYLON.PointLight("Omni", 
                                       new BABYLON.Vector3(0,100,100), 
                                       scene);
    var camera = new BABYLON.ArcRotateCamera("Camera", 0, 0.8, 100, 
                                             new BABYLON.Vector3.Zero(), 
                                             scene);
    var unit = BABYLON.Mesh.CreateBox("Unit", 6.0, scene);
    var origin = BABYLON.Mesh.CreateSphere("origin", 10, 1.0, scene);
    var groundMaterial = new BABYLON.StandardMaterial("ground", scene);
    groundMaterial.diffuseTexture = new BABYLON.Texture("terrainBlank.jpeg", scene);

//    var groundPlane = BABYLON.Mesh.CreatePlane("groundPlane", 1024.0, scene);
//    groundPlane.material = groundMaterial;

//    var ground = BABYLON.Mesh.CreateGroundFromHeightMap("ground", 
//                 "HeightMap1024x987.png", 1024, 987, 250, 0, 10, scene, false);
   
//    ground.material = groundMaterial;


        scene.activeCamera = camera;

    return scene;
}
