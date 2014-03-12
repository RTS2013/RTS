// revolutions per second
var angularSpeed = 0.02, 
    lastTime = 0;

// this function is executed on each animation frame
function animate(){
    // update
    var time = (new Date()).getTime(),
        timeDiff = time - lastTime,
        angleChange = angularSpeed * timeDiff * 2 * Math.PI / 1000;
    
    plane.rotation.z += angleChange;
    lastTime = time;

    // render
    renderer.render(scene, camera);

    // request new frame
    requestAnimationFrame(function(){
        animate();
    });
}
// renderer
var renderer = new THREE.WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

// camera
var camera = new THREE.PerspectiveCamera(45, 
                                         window.innerWidth / window.innerHeight, 
                                         1, 1000);
camera.position.y = -450;
camera.position.z = 400;
camera.rotation.x = 45 * (Math.PI / 180);

// scene
var scene = new THREE.Scene();

addPlane();

// start animation

animate();


