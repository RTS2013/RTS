init = function(){	

	if ( ! Detector.webgl ) Detector.addGetWebGLMessage();

	statsNode = document.getElementById('stats');

	container = document.getElementById( 'container' );

	scene.fog = new THREE.FogExp2( 0xfff4e5, 0.0003 );

	camera = new THREE.PerspectiveCamera( 45, SCREEN_WIDTH / SCREEN_HEIGHT, 
										   1, 10000 );
	camera.position.set( 0, 200, 100);
	
	camera.rotation.x = -Math.PI/4;

	scene.add(camera);

	stats.domElement.style.position = 'absolute';
	stats.domElement.style.top = '0px';
	container.appendChild( stats.domElement );

	controls = new RTSControls(camera, container);
	controls.movementSpeed = 100;

	//light = new THREE.DirectionalLight( 0xffffff, 0.05 );
	//light.position.set( 0, 100, 4 ).normalize();
	//scene.add( light );

	 plane = new THREE.Mesh(new THREE.PlaneGeometry(1000, 1000, 10, 10), 
	 							new THREE.MeshBasicMaterial({ 
	 									color: 0x808080, 
	 									wireframe: true 
	 								}));
		
	plane.rotation.x = -Math.PI / 2;
	plane.name = 'Ground';
	scene.add(plane);

	renderer = new THREE.WebGLRenderer( { antialias: true } );
	renderer.setClearColor( scene.fog.color, 1 );
	renderer.setSize( SCREEN_WIDTH, SCREEN_HEIGHT );
	renderer.sortObjects = false;

	container.appendChild( renderer.domElement );
	
	marker = new THREE.Mesh(new THREE.SphereGeometry(1), 
							new THREE.MeshLambertMaterial({ color: 0xff0000 }));
		scene.add(marker);

	//createUnits();

	//scene.add(dae);
	/*
	particleLight = new THREE.Mesh( new THREE.SphereGeometry( 4, 8, 8 ), new THREE.MeshBasicMaterial( { color: 0xffffff } ) );
	scene.add(particleLight);
	*/
	//container.addEventListener('mousedown', stopEvent, false);
	//container.addEventListener('mouseup', stopEvent, false);
	//container.addEventListener( 'mousemove', onMouseMove, false );
	container.addEventListener('click', clicked, false);
	window.addEventListener( 'resize', onWindowResize, false );

};