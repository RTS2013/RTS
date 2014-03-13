////////////////////////////
//Game class variables
/////////////////////////////
//storage containers for all the units and entities

var living	= new buckets.Dictionary(function (a){a.team + ":" + a.uid;});
var controlGroups = new Array(20);
var initX = 0, initY = 0, initZ = 100;

for(var i = 0; i < controlGroups.length; i++){

	controlGroups[1] = new buckets.Dictionary(function (a){a.team + ":" + a.uid;});

}

initGame = function(){	

	if ( ! Detector.webgl ) Detector.addGetWebGLMessage();

    statsNode = document.getElementById('stats');

	container = document.getElementById( 'container' );

	scene.fog = new THREE.FogExp2( 0xfff4e5, 0.0003 );

	camera = new THREE.PerspectiveCamera( 45, SCREEN_WIDTH / SCREEN_HEIGHT, 
										   1, 10000 );
	camera.position.set( initX, initY, initZ);
	
	camera.rotation.x = -Math.PI/4;

	scene.add(camera);

	stats.domElement.style.position = 'absolute';
	stats.domElement.style.top = '0px';
	container.appendChild( stats.domElement );

	controls = new RTSControls(camera, container);
	controls.movementSpeed = 100;

	renderer = new THREE.WebGLRenderer( { antialias: true } );
	renderer.setClearColor( scene.fog.color, 1 );
	renderer.setSize( SCREEN_WIDTH, SCREEN_HEIGHT );
	renderer.sortObjects = false;

	container.appendChild( renderer.domElement );
	
	marker = new THREE.Mesh(new THREE.SphereGeometry(1), 
							new THREE.MeshLambertMaterial({ color: 0xff0000 }));
	scene.add(marker);

	/*
	particleLight = new THREE.Mesh( new THREE.SphereGeometry( 4, 8, 8 ), new THREE.MeshBasicMaterial( { color: 0xffffff } ) );
	scene.add(particleLight);
	*/

	//container.addEventListener('mousedown', stopEvent, false);
	//container.addEventListener('mouseup', stopEvent, false);
	//container.addEventListener( 'mousemove', onMouseMove, false );
	container.addEventListener('click', controls.clicked, false);
	window.addEventListener( 'resize', controls.onWindowResize, false );

	this.createTerrain = createTerrain;
	this.createClouds = createClouds;

	loadAssets();
	createWorld();
	///////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////
	//							Game instance
	///////////////////////////////////////////////////////////////////////////

	// ...and here it is folks.  impressive huh?
	//var game = new Game();
};

function createWorld(){
	createTerrain();
	createLighting();
	createClouds();
}


function createTerrain(){
    // just a plane for now
    plane = new THREE.Mesh(new THREE.PlaneGeometry(1024, 1024, 32, 32), 
	 						new THREE.MeshBasicMaterial({ 
	 								color: 0x808080, 
	 								wireframe: true				
				}));
		
	//if we don't flip it 90 on the X axis it wil default to "facing" the camera
	plane.rotation.x = -Math.PI / 2;
	plane.name = 'Ground';
	scene.add(plane);
   	
}

//add clouds as a second SEPERATE object at same location
//but with different size, rotation, everything. 
function createClouds(){
	var cloudsMap = THREE.ImageUtils.loadTexture("images/Nebula-Clouds-1024.jpg");
	
	var cloudsMaterial = new THREE.MeshLambertMaterial({
		//change color attr to make the planet NON toxic
		//       R G B
		color: 0x00ff00, map: cloudsMap, transparent: true, opacity: 0.5
	});
	
	var cloudsGeometry = new THREE.PlaneGeometry(1024, 1024, 32, 32); /*32x32*/
    cloudsMesh = new THREE.Mesh( cloudsGeometry, cloudsMaterial );
	cloudsMesh.doubleSided = true;
	cloudsMesh.rotation.x = -Math.PI / 2;
	cloudsMesh.position.y = 300;
	cloudsMesh.name = 'Clouds';
	scene.add(cloudsMesh);
}

function createLighting(){
	//light = new THREE.DirectionalLight( 0xffffff, 0.05 );
	//light.position.set( 0, 100, 4 ).normalize();
	//scene.add( light );
}