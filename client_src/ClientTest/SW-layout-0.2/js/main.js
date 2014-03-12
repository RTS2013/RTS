var renderer =null,
	scene = null,
    camera = null,
    cube = null,
    animating = false;

function onLoad(){
	var container = document.getElementById("container");
	renderer = new THREE.WebGLRenderer({ antialias: true });
	renderer.setSize(container.offsetWidth, container.offsetHeight);
	container.appendChild(renderer.domElement);

	var cWidth = container.offsetWidth;
	var cHeight = container.offsetHeight;

	scene = new THREE.Scene();

	camera = new THREE.PerspectiveCamera( 45, cWidth/cHeight, 0.1, 10000 );
	camera.position.set(0,0,10);

	var theSun = new THREE.DirectionalLight(0xffffff, 1.5);
	theSun.position.set(0,0,1);
	scene.add(theSun);

    // Create a shaded, texture-mapped cube and add it to the scene
    // First, create the texture map
    var mapUrl = "js/images/testTexture.png";
    var map = THREE.ImageUtils.loadTexture(mapUrl);
    // Now, create a Phong material to show shading; pass in the map
    var material = new THREE.MeshPhongMaterial({ map: map });
    // Create the cube geometry
    var geometry = new THREE.CubeGeometry(1, 1, 1);
    // And put the geometry and material together into a mesh
    cube = new THREE.Mesh(geometry, material);
    // Turn it toward the scene, or we won't see the cube shape!
    cube.rotation.x = Math.PI / 5;
    cube.rotation.y = Math.PI / 5;
    // Add the cube to our scene
    scene.add( cube );
    // Add a mouse up handler to toggle the animation
    addMouseHandler();
    // Run our render loop
	run();
}

function run(){
	/* 
	*	put game update functions here
	*   "updateHandler();"
	*/
	// Spin the cube for next frame
    if (animating)
    {
        cube.rotation.y -= 0.01;
    }

    // Render the scene
    renderer.render( scene, camera );
    // Ask for another frame
    requestAnimationFrame(run);
}

onLoad();
