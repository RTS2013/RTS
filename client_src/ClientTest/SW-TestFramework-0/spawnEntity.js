// entityType needs to be passed in a an int.  this way we can use random
//numbers to control the models from the outside of this function
function spawnEntity(entityType, startX, startY, startZ, facingDir, friend){
	var entity;

	entity = modelArray[entityTpe].clone();
	entity.position.x = startX;
	entity.position.y = startY;
	entity.position.z = startZ;

	// * 360 * Math.PI/180; to convert to degrees if we need to
	entity.facing = entity.rotation.z = facingDir;

	//entity.z = passiveZ(entity.x, entity.y);
	//Im' pretty sure the Z should be controlled outside in the Game/world class
	
	entity.oldX = entity.position.x;
	entity.oldY = entity.position.y;
	entity.oldZ = entity.position.z;

	entity.entityType = entityType;

	if(friend){
		//put in the friend array
		friends.push(entity);
	} else {
		//put in the enemy array
		enemies.push(entity);
	}

 	scene.add(entity);
}