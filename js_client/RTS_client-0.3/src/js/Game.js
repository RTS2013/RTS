RTS.Game = function(game) {
	////////////////////////
	//Basic Game variables
	////////////////////////
	this.game = game;
/*
	if(this.game.device.desktop === false)
	{
		this.game.stage.scale.startFullScreen();
	}
*/
	SCREEN_W = DEVICE_W; SCREEN_H = DEVICE_H;

	STAGE_W = 3200; STAGE_H = 3200;

	GRID_W = 100; GRID_H = 100;

	//message passing class
	chef = null;

	//marquee select variables
	selectLine = null;
	selectBox = null;

	///////////////////////////
	//Level/Terrain variables
	///////////////////////////

	//Maps Vars
	tileMap = null;
	groundLayer = null;
	
	//Game over booleans
	gameOver = false;
	youLose = false;

	//image map and layer
	map = null;
	layer1 = null;

};

RTS.Game.prototype = {
	create: function() {
    	chef = new Chef();

    	this.game.stage.backgroundColor = '#2d2d2d';

    	this.game.world.setBounds(0, 0, 3200, 3200);

    	this.game.add.sprite(0, 0, 'seamlessConcreteHUGE');
    	this.game.add.sprite(0, 1600, 'seamlessConcreteHUGE');
    	this.game.add.sprite(1600, 0, 'seamlessConcreteHUGE');
    	this.game.add.sprite(1600, 1600, 'seamlessConcreteHUGE');

    	//puts down the little bunkers
    	this.populate();

//////////////////////////////////////////////////////////////////begin
/*
		//I don't think I need this but I'm keeping it for now.
		//I know I figured out how to get the world coordinates of a touchpoint but 
		//I'm still not exactly sure how to dynamically draw the rectangle
	    //  Creates a blank tilemap
	    map = this.game.add.tilemap();

	    //  Add a Tileset image to the map
	    map.addTilesetImage('ground_1x1');

	    //  Creates a new blank layer and sets the map dimensions.
	    //  In this case the map is 40x30 tiles in size and the tiles are 32x32 pixels in size.
	    layer1 = map.create('level1', 100, 100, 32, 32);

	    //slow scroll factor by 50%.  used for paralaxing
	    //layer1.scrollFactorX = 0.5;
	    //layer1.scrollFactorY = 0.5;

	    //  Resize the world
	    layer1.resizeWorld();
*/
////////////////////////////////////////////////////////////////////end

	    //create HUD Function
		HUD.createHUD();
		KB.init();
		
	},

	update: function() {
		KB.listen();
	    IN.listen();
	    EF.listen();
	},

	populate: function(){
    	for(var i = 0; i < 3200; i+=32){
    		for(var j = 0; j < 3200; j+=32){
    			if(i % 512 == 0 && j % 1024 == 0){
    				this.game.add.sprite(i, j, 'bevBrownTile64');
    			}
    			if(j % 768 == 0 && i % 256 == 0){
    				this.game.add.sprite(i, j, 'regBrownTile64');
    			}
    		}
    	}
	},

	sendMoveCommand: function(chef,unitIDArray,x,y,queue){
  		chef.putU8(1);
  		chef.putU8(queue);
  		chef.putU16(unitIDArray.length);
  		for (i = 0; i < unitIDArray.length; i++) {
   			chef.put32(unitIDArray[i]);
  		}
  		chef.trim();
  	},

	render: function() {

	}//,

  	/*
  	//GOTTA MAKE THIS MY OWN!!!!!!
	getItemByUid:function(uid){
	    for (var i = game.items.length - 1; i >= 0; i--){
	        if(game.items[i].uid == uid){
	            return game.items[i];
	        }
	    };
	},
	*/
	/*
	spawnMine: function() {

		var m = mineGroup.create(this.rnd.integerInRange(0, 640-158), 960, 'mine');
		this.physics.enable(m, Phaser.Physics.ARCADE);
		m.body.velocity.y = -200*speed+this.rnd.integerInRange(0, 100);
		m.body.velocity.x = this.rnd.integerInRange(-100, 100);
		m.cachedVelocity = {};

	},
	killSprite: function(unit) {

		unitGroup
		starsText.setText(starsCount);
		star.kill();

	},

	restartGame: function() {
		this.add.tween(gameOverText).to( { alpha: 0 }, 300, Phaser.Easing.Linear.None, true, 0, 0, true);
		this.add.tween(buttonBack).to({x: -160}, 1000, Phaser.Easing.Elastic.Out, true, 0, false);
		var restartTween = this.add.tween(buttonRestart);
		restartTween.to({ x: 640 }, 1000, Phaser.Easing.Elastic.Out);
		restartTween.onComplete.addOnce(function(){
			this.state.start('Game');
		}, this);
		restartTween.start();

	},

	parallax: function() {

		bgSeaweed.y -= (speed/2);
		bgAnchors.y -= (speed/4);
		bgMines.y -= (speed/6);
		// if(seaweed.y == -2880) seaweed.y = 0;
		if(this.bgBubbles) {
			this.bgBubbles.y -= speed*4;
		}

	},
	*/
};