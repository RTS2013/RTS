//CLASS OBJECTS FOR "Earth" = MOON + EARTH OBJECT HIERARCHY
GameMaster = function(){
	Sim.Object.call(this);
}

GameMaster.prototype = new Sim.Object();

GameMaster.prototype.init = function(){
	var controlGroups = new Array(20);

	for(var i = 0; i < controlGroups.length; i++){

		controlGroups[i] = THREE.Object3D();
	}
	//tell framework about the object
	this.setObject3D(earthGroup);
	//
	this.createGlobe();
	this.createClouds();

	this.createMoon();
}
//Create earth and textures
Earth.prototype.createGlobe = function(){
	var surfaceMap = THREE.ImageUtils.loadTexture("../images/earth_surface_2048.jpg");
	var normalMap = THREE.ImageUtils.loadTexture("../images/earth_normal_2048.jpg");
	var specularMap = THREE.ImageUtils.loadTexture("../images/earth_specular_2048.jpg"); 

	var shader = THREE.ShaderUtils.lib["normal"],
	uniforms = THREE.UniformsUtils.clone(shader.uniforms);

	uniforms[ "tNormal" ].texture = normalMap;
    uniforms[ "tDiffuse" ].texture = surfaceMap;
    uniforms[ "tSpecular" ].texture = specularMap;
    uniforms[ "enableDiffuse" ].value = true;
    uniforms[ "enableSpecular" ].value = true;
    
    var shaderMaterial = new THREE.ShaderMaterial({
        fragmentShader: shader.fragmentShader,
        vertexShader: shader.vertexShader,
        uniforms: uniforms,
        lights: true
    });
    
    var globeGeometry = new THREE.SphereGeometry(1, 32, 32);
    // We'll need these tangents for our shader
    globeGeometry.computeTangents();
    var globeMesh = new THREE.Mesh( globeGeometry, shaderMaterial );
    // Let's work in the tilt
    globeMesh.rotation.z = Earth.TILT;
    // Add it to our group
    this.object3D.add(globeMesh);
    // Save it away so we can rotate it
    this.globeMesh = globeMesh;    	
}

//add clouds as a second SEPERATE object at same location
//but with different size, rotation, everything.  they are seperate
//just laid over each other to give the effect of one-ness
Earth.prototype.createClouds = function(){
	var cloudsMap = THREE.ImageUtils.loadTexture("../images/earth_clouds_1024.png");
	
	var cloudsMaterial = new THREE.MeshLambertMaterial({
		//change color attr to make the planet NON toxic
		//       R G B
		color: 0x00ff00, map: cloudsMap, transparent: true
	});
	
	var cloudsGeometry = new THREE.SphereGeometry(Earth.CLOUDS_SCALE, 32, 32);
    cloudsMesh = new THREE.Mesh( cloudsGeometry, cloudsMaterial );
    cloudsMesh.rotation.z = Earth.TILT;
    // Add it to our group
    this.object3D.add(cloudsMesh);
    // Save it away so we can rotate it
    this.cloudsMesh = cloudsMesh;	
}
//create the Moon object that we will "child" to the Earth 
//in the object hierarchy
Earth.prototype.createMoon = function(){
	var moon = new Moon();
	moon.init();
	this.addChild(moon);
}

//METHODS FOR CLASS
Earth.prototype.update = function(){
	// "I feel the Earth move..."
	this.object3D.rotation.y += Earth.ROTATION_Y;
	//clouds too
	this.cloudsMesh.rotation.y += Earth.CLOUDS_ROTATION_Y;

	Sim.Object.prototype.update.call(this);
}


//Class/Object data members.  "pubic scope"
Earth.ROTATION_Y = 0.0025;
Earth.TILT = 0.41;
Earth.RADIUS = 6371;
Earth.CLOUDS_SCALE = 1.01;
Earth.CLOUDS_ROTATION_Y =  Earth.ROTATION_Y * 0.95;
