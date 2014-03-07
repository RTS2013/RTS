
//storage containers for all the
var friends = new Array();
var enemies = new Array();
var neutrals = new Array();

//Unit initial values
var	BASE_LIFE,
	BASE_MELEE,
	BASE_RANGE,
	BASE_SPEED,
	BASE_SIGHT,
	BASE_ARMOR,
	BASE_FEAR,
	BASE_ROLE;

//Map initial values
var WORLD_WIDTH,
	WORLD_DEPTH,
	START_WIDTH,
	START_DEPTH;


initGame = function(){
	//should take initialization data from the Chef
    loadAssets();

    var CL = new ChefListener();

	BASE_LIFE  = CL.base_life;
	BASE_MELEE = CL.base_melee;
	BASE_RANGE = CL.base_range;
	BASE_SPEED = CL.base_speed;
	BASE_SIGHT = CL.base_sight;
	BASE_ARMOR = CL.base_armor;
	BASE_FEAR  = CL.base_fear;
	BASE_ROLE  = CL.base_role;

	WORLD_WIDTH = CL.map_x;
	WORLD_DEPTH = CL.map_y;

	START_WIDTH = CL.start_width;
	START_DEPTH = CL.start_height;

	CL.createStartScene();

};

startTerrain = function(){
	//this will stamp out the terrain's z coordinates for the player's start position
	//really all it does is call the smaller revealTerrain function in a loop

	for(var i = 0; i < ){

	}
}

