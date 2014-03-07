var numEntities;

function spwanEntity(entityType, startX, startY){
	this.x = startX;
	this.y = startY;
	this.z = passiveZ(this.x, this.y);

	this.oldX = startX;
	this.oldY = startY;
	this.entityType = entityType;
}