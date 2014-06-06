Unit = function(x, y, facing, sprite) {
  this.posX = x;
  this.posY = y;
  this.facing = facing;

  this.doing = 'wait'; 

  this.sprite = sprite;

  //this.init();
};

Unit.prototype = {
	
  	init: function () {
    	//this.listeners();
  	  	this.sprite = game.add.sprite(this.posX, this.posY, this.sprite);
  	},

  	setFacing: function(facing){
  		this.facing = facing;
  	},

  	setDoing: function(doing){
  		this.doing = doing;
  	},

  	setPos: function(x, y){
  		this.posX = x;
  		this.posY = y;
  	},

	//getUnitByAction(string action){return unit}
	getUnitByAction:function(doing){
	    for (var i = EF.myUnitCount - 1; i >= 0; i--){
	        if(EF.myUnitArray[i].doing == doing){
	            return EF.myUnitArray[i];
	        }
	    };
	}//,
};