window.onload = function(){

var canvas = document.getElementById("renderCanvas");
  
    if(!BABYLON.Engine.isSupported()){
        window.alert("Game doesn't support browser");
    } else {
        // Load BABYLON 3D engine and set the root directory

        // 1- Create a new scene with 
        //       a camera (mandatory), 
        //       a light  (better) and 
        //       a sphere (to see the origin)
        // 2- Creating a camera looking to the zero point (0,0,0)
        // 3- Creating a omnidirectional light
        // 4- Creating a sphere of size 1, at 0,0,0
        // 5- Attach the camera to the scene
        // 6- Once the scene is loaded, just register a render loop to render it
        
//        scroller.listen();

    var engine = new BABYLON.Engine(canvas, true);
        //  scene var is from "scene.js"
        scene  = initializeScene(engine);

        engine.runRenderLoop(function () {
            scene.render();
        

        });
        
    }
};
