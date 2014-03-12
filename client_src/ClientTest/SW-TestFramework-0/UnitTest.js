Unit = function(){
	Sim.Object.call(this);
};

Unit.prototype = new Sim.Object();
//Just to make sure, if you didn't see it in spawnEntity, we want to pass entityType as 
//an int because it allows us to do random selection between certain models
Unit.prototype.init = function(entityType, startX, startY, startZ, facingDir, friend){
	param = param || {};

	var unitControlGroup = new THREE.Object3D();
	this.setObject3D(unitControlGroup);
	

	var unitGroup = new THREE.Object3D();
	
	var distance = param.distance || 0;
	
	var distSquared = distance * distance;
	
	unitGroup.position.set(Math.sqrt(distSquared/2), 0, -Math.sqrt(distSquared/2));
	
	unitControlGroup.add(unitGroup);

	this.friend = friend
	this.unitGroup = unitGroup;
	var size = param.size || 1;
	this.unitGroup.scale.set(size, size, size);

	this.unitGroup.rotation.x = Saturn.TILT;	
	this.createGlobe();
	this.createRings();

	this.animateOrbit = param.animateOrbit;
	this.period = param.period;
	this.revolutionSpeed = param.revolutionSpeed ? param.revolutionSpeed : Saturn.REVOLUTION_Y;
};
Unit.prototype.create = function(friend){
	var that = this;
	var unitMesh = spawnEntity(that.entityType);

	this.unitGroup.add(unitMesh);
	this.mesh = unitMesh;
};

Unit.prototype.update = function(){

	Sim.Object.prototype.update.call(this);
};

Unit.SPEED = 10;
Unit.HP = 100;



