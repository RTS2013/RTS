// Custom Sun class
Sun = function(){
	Sim.Object.call(this);
}

Sun.prototype = new Sim.Object();

Sun.prototype.init = function(){
	// Create a group to hold our sun mesh and light
	var sunGroup = new THREE.Object3D();
	var sunMap = "../images/sun_surface.jpg";
	var texture = THREE.ImageUtils.loadTexture(sunMap);
    var material = new THREE.MeshLambertMaterial({ 
    					map: texture, 
    					ambient : 0xffff00,
    					transparent : true, 
    					opacity : 0.25
    				});

    var geometry = new THREE.SphereGeometry(Sun.SIZE_IN_EARTHS, 64, 64);
	sunMesh = new THREE.Mesh( geometry, material );
	
	//var light = new THREE.PointLight( 0xffffff, 1.2, 1000 );
	//sunGroup.add(light);
	sunGroup.add(sunMesh);
	//	this.addLight(light);
    this.setObject3D(sunGroup);
	sunGroup.position.x += 20;        
}
/*
Sun.prototype.addLight = function(light){

	//var light = new THREE.PointLight( 0xffffff, 1.2, 1000 );
	this.addChild(light);
}

Sun.prototype.update = function(){
	//donothing for now but it's needed for the 
	//Sim.js framework OBJ type
	Sim.Object.prototype.update.call(this);
}
*/
Sun.SIZE_IN_EARTHS = 10;
