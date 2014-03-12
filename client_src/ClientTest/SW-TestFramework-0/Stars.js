Stars = function(){
	Sim.Object.call(this);
}

Stars.prototype = new Sim.Object();

Stars.prototype.init = function(minDistance){
	var starsGroup = new THREE.Object3D();
	var starsGeometry = new THREE.Geometry();
	var i;

	for(i = 0; i < Stars.N_VERTICES; i++){
		var vector = new THREE.Vector3(
							(Math.random() * 2 - 1) * minDistance,
							(Math.random() * 2 - 1) * minDistance,
							(Math.random() * 2 - 1) * minDistance
						);
		if(vector.length() < minDistance){
			vector = vector.setLength(minDistance);
		}
		starsGeometry.vertices.push(new THREE.Vertex(vector));
	}

	var starsMaterials = [];
	for(i = 0; i < Stars.N_MATERIALS; i++){
		starsMaterials.push(
			new THREE.ParticleBasicMaterial({
				color: 0x101010 * (i+1),
				size: i % 2 + 1,
				// this stops the stars from resizing as their distance from the
				// camera changes
				sizeAttenuation: false 
			})
		);
	}

	for(i = 0; i < Stars.N_PARTICLES_SYSTEMS; i++){
		var stars = new THREE.ParticleSystem(starsGeometry, starsMaterials[i % Stars.N_MATERIALS]);
		stars.rotation.y = i / (Math.PI * 2);
		starsGroup.add(stars);
	}
	this.setObject3D(starsGroup);
}
Stars.N_VERTICES = 667;
Stars.N_MATERIALS = 8;
Stars.N_PARTICLE_SYSTEMS = 24;

