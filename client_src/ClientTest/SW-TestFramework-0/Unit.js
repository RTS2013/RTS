Unit = function(){
	Sim.Object.call(this);
}

Unit.prototype = new Sim.Object();

Unit.prototype.init = function(entityType, startX, startY, startZ, facingDir){

	spawnEntity(entityType, startX, startY, startZ, facingDir);
}

Unit.prototype.update = function(){
	this.takeCommand();
	Sim.Object.prototype.update.call(this);
}

Unit.prototype.takeCommand = function(){
	//this is where the ChefListener feeds into
	console.log("takeCommand executed");
}

//stats for the units that are universal.
//BUT REALLY these should come from world constants that initGame initializes.
Unit.prototype.stats = {
	health 		= BASE_LIFE || this.healthBonus;
	melee 		= BASE_MELEE || this.meleeBonus;
	ranged		= BASE_RANGE || this.rangeBonus;
	speed 		= BASE_SPEED || this.speedBonus;
	sightRad 	= BASE_SIGHT || this.sightBonus;

	//type based:  1-10 or whatever.  5 is medium and gives you whatever we decide.  
	//like unnamed ENUMS
	armor		= BASE_ARMOR || this.armorBonus;

	//feear from 0-100%
	unit.fear 	= BASE_FEAR;

};



