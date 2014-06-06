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

	//HUD VARIABLES
	displayText = null;
	
	//marquee select variables
	selectLine = null;
	selectBox = null;

	//touch control variables
	cursors = null;

	touchData = {
		oldX: 10, oldY: 10,
		nowX: 40, nowY: 40,
		//w: Math.abs(this.oldX - this.nowX),
		//h: Math.abs(this.oldY - this.nowY)
		w: 0, h: 0,
		centerX: 0.0, centerY: 0.0,
		touchDrag: false,
		touchDown: false,
		touchUp: true
	};

    screenQuads = {
		screenHalfX: (SCREEN_W/2),
		screenHalfY: (SCREEN_H/2),
		atCenterX: false,
		atCenterY: false
	};

	///////////////////////////
	//Level/Terrain variables
	///////////////////////////

	//Maps Vars
	tileMap = null;
	groundLayer = null;
	
	myStructArray = [];
	//friendStructArray = [];
	enemyStructArray = [];
	neutralScructArray = [];

	//for ambient wind blowing and rats scurrying;  pretty much just sprite terrain
	passiveSpriteArray = [];
	
	//Unit groups
	myUnitArray = [];
	myUnitCount = 0;

	enemyUnitArray = [];
	enemyUnitCount = 0;

	//friendUnitArray = [];
	//friendUnitCount = 0;

	
	//Game over booleans
	gameOver = false;
	youLose = false;

	//UI buttons

	//action (left hand)
	aButton = null;
	bButton = null;
	eButton = null;

	//control groups
	ctrlGroupBtn1 = null; ctrlGroupBtn2 = null; ctrlGroupBtn3 = null; ctrlGroupBtn4 = null; ctrlGroupBtn5 = null;
	ctrlGroupBtn6 = null; ctrlGroupBtn7 = null; ctrlGroupBtn8 = null; ctrlGroupBtn9 = null; ctrlGroupBtn10 = null;

	//mini map
	miniMap = null;

	//image map and layer
	map = null;
	layer1 = null;

};

RTS.Game.prototype = {
	create: function() {
    	chef = new Chef();

    	this.game.stage.backgroundColor = '#2d2d2d';

    	// Stretch to fill
    	//this.game.stage.scale.fullScreenScaleMode = Phaser.ScaleManager.EXACT_FIT;
    	//this.game.stage.scale.startFullScreen();
    	
    	//stretch/update technique
	    //this.game.stage.fullScreenScaleMode = Phaser.ScaleManager.SHOW_ALL;

    	this.game.world.setBounds(0, 0, 3200, 3200);

    	this.game.add.sprite(0, 0, 'seamlessConcreteHUGE');
    	this.game.add.sprite(0, 1600, 'seamlessConcreteHUGE');
    	this.game.add.sprite(1600, 0, 'seamlessConcreteHUGE');
    	this.game.add.sprite(1600, 1600, 'seamlessConcreteHUGE');

    	//puts down the little bunkers
    	this.populate();

		console.log(""+ Math.abs(touchData.oldY - touchData.nowY));

		console.log("screen half Y" + screenQuads.screenHalfY);
    	console.log("screen half X" + screenQuads.screenHalfX);

    	/*
		window.addEventListener('resize', function(event){
    		resizeGame();
		});
		*/
//////////////////////////////////////////////////////////////////begin

    	//I think I have to give it a tile map in order for it to register touch events on the level.
    	//
    	//It would also make selcting much easier because I could simply query the tile's coordinates.
    	//Grabbing all the units who fall in the given area.

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

////////////////////////////////////////////////////////////////////end

    	cursors = this.game.input.keyboard.createCursorKeys();
    	cursors.SHIFT = Phaser.Keyboard.SHIFT;

    	/*
    	//Moved to the marquee select function in update
		selectBox = this.game.add.graphics(100,100);

		selectBox.fixedToCamera = true;
    	
    	selectBox.lineStyle(2, 0x00ff00, 1);
    	selectBox.drawRect(0, 0, 32, 32);
    	*/

	    //create HUD Function
		this.createHUD();
		
	},

	update: function() {
		//animate units
		for(var i = 0; i < myUnitCount; i++){
			myUnitArray[i].animations.play('walk');
		}
		for(var i = 0; i < enemyUnitCount; i++){
			enemyUnitArray[i].animations.play('walk');
		}

		//keyboard handler for screen movement
		if (cursors.up.isDown){
	        this.game.camera.y -= 24;
	    }
	    else if (cursors.down.isDown){
	        this.game.camera.y += 24;
	    }
	    if (cursors.left.isDown){
	        this.game.camera.x -= 24;
	    }
	    else if (cursors.right.isDown){
	        this.game.camera.x += 24;
	    }

	    if(this.game.input.keyboard.isDown(cursors.SHIFT)){
	    	console.log("SHIFT is being pressed");

	    	////////////////start here///////////////////
	    	////////////////////////////////////////////
	    }

	    //Mouse pointer
	    /*
	    if (this.game.input.mousePointer.isDown){
	        //touchData.nowX = this.game.input.mousePointer.x;
	        //touchData.nowY = this.game.input.mousePointer.y;
	        //console.log("touch data X, Y: (" + touchData.nowX + ", "+ touchData.nowY +")");
	    	//console.log("mouseDown!!!");
	    }
	    else if (this.game.input.pointer1.isDown && this.game.input.pointer2.isDown){
	        //touchData.nowX = this.game.input.pointer1.x;
	        //touchData.nowY = this.game.input.pointer1.y;
	        //console.log("touch data X, Y: (" + touchData.nowX + ", "+ touchData.nowY +")");
	    	//console.log("Pointer 1 Down!!!");
	    }

	    if (this.game.input.pointer2.isDown){
	        //touchData.nowX = this.game.input.pointer1.x;
	        //touchData.nowY = this.game.input.pointer1.y;
	        //console.log("touch data X, Y: (" + touchData.nowX + ", "+ touchData.nowY +")");
	    	//console.log("Pointer 2 Down!!!");
	    }

	    if (this.game.input.pointer3.isDown){
	        //touchData.nowX = this.game.input.pointer1.x;
	        //touchData.nowY = this.game.input.pointer1.y;
	        //console.log("touch data X, Y: (" + touchData.nowX + ", "+ touchData.nowY +")");
	    	//console.log("Pointer 3 Down!!!");
	    }
		*/

		/*
		//Screen drag functionality
		this.game.input.onDown.add(function(e){
		
		    if (Math.round(e.x/SCREEN_W) === 1) {
		       //console.log("Right side onTap Registered");
		       this.game.camera.x += 4;
		    }
		    if (Math.round(e.y/SCREEN_H) === 1) {
		       //console.log("Right side onTap Registered");
		       this.game.camera.y += 4;
		    }		
		    if (Math.round(e.x/SCREEN_W) === 0){
		        //console.log("Left side onTap Registered");
		        this.game.camera.x -= 4;
		    }
		    if (Math.round(e.y/SCREEN_H) === 0){
		        //console.log("Left side onTap Registered");
		    	this.game.camera.y -= 4;
		    }
		});
		*/
		//MARQUEE SELECT IS ALL F'd UP
		if (this.game.input.pointer1.isDown && this.game.input.pointer2.isDown){

			touchData.oldX = this.game.input.pointer1.worldX;
			touchData.oldY = this.game.input.pointer1.worldY;
			touchData.nowX = this.game.input.pointer2.worldX;
			touchData.nowY = this.game.input.pointer2.worldY;
			/*
			touchData.oldX = this.game.input.pointer1.x;
			touchData.oldY = this.game.input.pointer1.y;
			touchData.nowX = this.game.input.pointer2.x;
			touchData.nowY = this.game.input.pointer2.y;
			*/
			touchData.w = Math.abs(touchData.oldX - touchData.nowX);
			touchData.h = Math.abs(touchData.oldY - touchData.nowY);
			
			/*
		    console.log("selectBox.bottom : " + selectBox.bottom);
		    console.log("selectBox.right : " + selectBox.right);
		    console.log("selectBox.top : " + selectBox.top);
		    console.log("selectBox.left : " + selectBox.left);

		    console.log("selectBox.x : " + selectBox.x);
		    console.log("selectBox.y : " + selectBox.y);
			*/

		    //selectBox.width = touchData.w;
		    //selectBox.height = touchData.h;
		    
		    var maxY = Math.max(touchData.oldY, touchData.nowY);
		    var minY = Math.min(touchData.oldY, touchData.nowY);
		    var maxX = Math.max(touchData.oldY, touchData.nowY);
		    var minX = Math.min(touchData.oldY, touchData.nowY);

			selectBox = this.game.add.graphics(minX,minY);
			selectBox.visible = false;
    	
    	
    		selectBox.lineStyle(2, 0x00ff00, 1);
    		selectBox.drawRect(minX, minY, touchData.h, touchData.w);

 			selectBox.fixedToCamera = true;
			
			console.log("touchData.w : " + touchData.w);
			
		}
		//else do this
	else {
		if(this.game.input.activePointer.isDown){


			touchData.oldX = this.game.input.pointer1.x;
			touchData.oldY = this.game.input.pointer1.y;
			
			//touchData.nowX = this.game.input.pointer2.x;
			//touchData.nowY = this.game.input.pointer2.y;

			//touchData.w = Math.abs(touchData.oldX - touchData.nowX);
			//touchData.h = Math.abs(touchData.oldY - touchData.nowY);

			touchData.centerY = touchData.oldY/SCREEN_H;
			touchData.centerX = touchData.oldX/SCREEN_W;

			if(touchData.centerX > 0.33 && touchData.centerX < 0.66){
		       	screenQuads.atCenterX = true;
		    }

			if(touchData.centerY > 0.33 && touchData.centerY < 0.66){
		       	screenQuads.atCenterY = true;
		    }

			//up
			 if(Math.round(touchData.centerY) === 1) {
			 	if(screenQuads.atCenterX){
			 		//do nothing
			 		//this.game.camera.y += 4;
			 	}else{
		       		this.game.camera.y += 4;
		    	}
		    	screenQuads.atCenterX = false;
		    }

		    //down
		    if(Math.round(touchData.centerY) === 0) {
		       	if(screenQuads.atCenterX){
			 		//do nothing
			 		//this.game.camera.y -= 4;
			 	}else{
		       		this.game.camera.y -= 4;
			    }
			    screenQuads.atCenterX = false;
		    }	

		    //left	
		    if(Math.round(touchData.centerX) === 0){
			 	if(screenQuads.atCenterY){
			 		//do nothing
			 		//this.game.camera.x -= 4;
			 	}else{
		        	this.game.camera.x -= 4;
		    	}
		    	screenQuads.atCenterY = false;
		    }

		    //right
		    if(Math.round(touchData.centerX) === 1){
			 	if(screenQuads.atCenterY){
			 		//do nothing
			 		//this.game.camera.x += 4;
			 	}else{
		    		this.game.camera.x += 4;
		    	}
		    	screenQuads.atCenterY = false;
			}
			/*
		    //move action once vars are set
			if(touchData.moveUp){
				this.game.camera.y += 10;
			}
			if(touchData.moveDown){
				this.game.camera.y -= 10;
			}
			if(touchData.moveLeft){
				this.game.camera.x -= 10;
			}
			if(touchData.moveRight){
				this.game.camera.x += 10;
			}
			*/

		}
	}
		/*
		//new screen pan dragging attempt
		this.game.input.onDrag.add(function(e){
		
		    if (Math.round(e.x/SCREEN_W) === 1) {
		       //console.log("Right side onTap Registered");
		    }			
		    if (Math.round(e.x/SCREEN_W) === 0){
		        //console.log("Left side onTap Registered");
		});
		*/

		/*
		//try at marquee select   
		if (drawingLine){
        	if (this.game.input.activePointer.isDown){
            	//selectLine.end.set(this.game.input.activePointer.x, this.game.input.activePointer.y);
        		console.log("drawing line is true");
        	}else{
            	settingLine = false;
            	//console.log("drawing line is false");
        	}
    	}
		*/



		/*
		//touch panning HOPEFULLY
    	if(this.game.input.activePointer.isDown){
    		if(!touchData.touchDrag){

    			touchData.oldX = this.game.input.activePointer.x;
    			touchData.oldY = this.game.input.activePointer.y;
    			console.log("DOWN touchData X/Y: "+touchData.oldX + ", "+touchData.oldY);
	    		
	    		touchData.touchDrag = true;
	    	}
    	}

    	if(this.game.input.activePointer.isUp){
    		if(touchData.touchDrag){
    			touchData.touchDrag = false;
    			touchData.nowX = this.game.input.activePointer.x;
    			touchData.nowY = this.game.input.activePointer.y;

    			console.log("UP touchData X/Y: "+touchData.nowX + ", "+touchData.nowY);

    			if(touchData.oldX - touchData.nowX < 0){
					this.game.camera.x -= Math.abs(touchData.oldX - touchData.nowX);
					//this.game.camera.x -= 20;
				}
				else{
					this.game.camera.x += Math.abs(touchData.oldX - touchData.nowX);
					//this.game.camera.x += 20;
				}
				if(touchData.oldY - touchData.nowY < 0){
					this.game.camera.y -= Math.abs(touchData.oldX - touchData.nowX);
					//this.game.camera.y -= 20;   			
				}
				else{
					this.game.camera.y += Math.abs(touchData.oldX - touchData.nowX);
					//this.game.camera.y += 20; 
				}

				touchData.oldX = this.game.input.activePointer.x;
    			touchData.oldY = this.game.input.activePointer.y;
    		}
    	}

		*/
	},

	click: function(pointer) {
    	drawingLine = true;
    	console.log(""+selectBox);
   	 	//selectLine.start.set(pointer.x, pointer.y);	
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

	createHUD: function() {
		// HUD
    	this.game.input.addPointer();

    	this.game.add.text(10, 10, "- phaser test text -", { font: "32px Arial", fill: "#330088", align: "center" });

    	//Left Hand buttons

	    aButton = this.game.add.button(10, SCREEN_H - 74, 'metalButton64', this.actionOnClickA, this, 2, 1, 0);
    	aButton.inputEnabled = true;
    	aButton.fixedToCamera = true;
    	
    	bButton = this.game.add.button(10 + 64 + 10, SCREEN_H - 74, 'metalButton64', this.actionOnClickB, this, 2, 1, 0);
    	bButton.inputEnabled = true;
    	bButton.fixedToCamera = true;
    	
    	eButton = this.game.add.button(10 + 128 + 20, SCREEN_H - 74, 'metalButton64', this.actionOnClickE, this, 2, 1, 0);
    	eButton.inputEnabled = true;
    	eButton.fixedToCamera = true;

    	//Control Group Buttons

    	ctrlGroupBtn1 = this.game.add.button(SCREEN_W - 320 - 100, 10, 'metalButton32', this.selectCG1, this, 2, 1, 0);
		ctrlGroupBtn1.inputEnabled = true;
    	ctrlGroupBtn1.fixedToCamera = true;

    	ctrlGroupBtn2 = this.game.add.button(SCREEN_W - 288 - 90, 10, 'metalButton32', this.selectCG2, this, 2, 1, 0);
		ctrlGroupBtn2.inputEnabled = true;
    	ctrlGroupBtn2.fixedToCamera = true;
    	
    	ctrlGroupBtn3 = this.game.add.button(SCREEN_W - 256 - 80, 10, 'metalButton32', this.selectCG3, this, 2, 1, 0);
		ctrlGroupBtn3.inputEnabled = true;
    	ctrlGroupBtn3.fixedToCamera = true;
    	    	
    	ctrlGroupBtn4 = this.game.add.button(SCREEN_W - 224 - 70, 10, 'metalButton32', this.selectCG4, this, 2, 1, 0);
		ctrlGroupBtn4.inputEnabled = true;
    	ctrlGroupBtn4.fixedToCamera = true;
    	    	
    	ctrlGroupBtn5 = this.game.add.button(SCREEN_W -192 - 60, 10, 'metalButton32', this.selectCG5, this, 2, 1, 0);
		ctrlGroupBtn5.inputEnabled = true;
    	ctrlGroupBtn5.fixedToCamera = true;
    	    	
    	ctrlGroupBtn6 = this.game.add.button(SCREEN_W - 160 - 50, 10, 'metalButton32', this.selectCG6, this, 2, 1, 0);
		ctrlGroupBtn6.inputEnabled = true;
    	ctrlGroupBtn6.fixedToCamera = true;
    	    	
    	ctrlGroupBtn7 = this.game.add.button(SCREEN_W - 128 - 40, 10, 'metalButton32', this.selectCG7, this, 2, 1, 0);
		ctrlGroupBtn7.inputEnabled = true;
    	ctrlGroupBtn7.fixedToCamera = true;
    	    	
    	ctrlGroupBtn8 = this.game.add.button(SCREEN_W - 96 - 30, 10, 'metalButton32', this.selectCG8, this, 2, 1, 0);
		ctrlGroupBtn8.inputEnabled = true;
    	ctrlGroupBtn8.fixedToCamera = true;
		
    	ctrlGroupBtn9 = this.game.add.button(SCREEN_W - 64 - 20, 10, 'metalButton32', this.selectCG9, this, 2, 1, 0);
		ctrlGroupBtn9.inputEnabled = true;
    	ctrlGroupBtn9.fixedToCamera = true;

    	ctrlGroupBtn10 = this.game.add.button(SCREEN_W - 32 - 10, 10, 'metalButton32', this.selectCG10, this, 2, 1, 0);
		ctrlGroupBtn10.inputEnabled = true;
    	ctrlGroupBtn10.fixedToCamera = true;
		
    	/*
2.2 + 32 + 10, 10, 
2.2 + 64 + 20, 10, 
2.2 + 96 + 30, 10, 
2.2 + 128 + 40, 10,
2.2 + 160 + 50, 10,
2.2 + 192 + 60, 10,
2.2 + 224 + 70, 10,
2.2 + 256 + 80, 10,
2.2 + 288 + 90, 10
		*/

		console.log("HUD is working ");
	},
	actionOnClickA: function(){
		this.spawnMyUnit();
	},
	actionOnClickB: function(){
		console.log("I don't do anything says the re-re button");

	},
	actionOnClickE: function(){
		this.spawnEnemyUnit();
	},
	selectCG1: function(){
		console.log("control group 1 clicked");
	},
	selectCG2: function(){
		console.log("control group 2 clicked");
	},
	selectCG3: function(){
		console.log("control group 3 clicked");	
	},
	selectCG4: function(){
		console.log("control group 4 clicked");	
	},
	selectCG5: function(){
		console.log("control group 5 clicked");	
	},
	selectCG6: function(){
		console.log("control group 6 clicked");	
	},
	selectCG7: function(){
		console.log("control group 7 clicked");	
	},
	selectCG8: function(){
		console.log("control group 8 clicked");	
	},
	selectCG9: function(){
			console.log("control group 9 clicked");
	},
	selectCG10: function(){
			console.log("control group 10 clicked");
	},

	spawnEnemyUnit: function() {
		//these functions actually need to take spawn points but whatever for now

		var spawnY = Math.floor(Math.random() * (SCREEN_H + 1));
		var spawnX = Math.floor(Math.random() * (SCREEN_W + 1));
		console.log("spawnX: "+spawnX + ", spawnY: "+spawnY);

		enemyUnitArray[enemyUnitCount] = this.game.add.sprite(spawnX, spawnY, 'testUnitImg');

		enemyUnitArray[enemyUnitCount].animations.add('walk', [0, 1, 2, 3], 10, true);

		//setting enemy angle to check for degs or rads.  Its DEGREES.
		enemyUnitArray[enemyUnitCount].angle = 180; 

		enemyUnitCount++;
		console.log("spawned enemy unit");
		//myUnitGroup.create(spawnX, spawnY, 'testUnit');
	},

	spawnMyUnit: function() {
		//these functions actually need to take spawn points but whatever for now
		var spawnY = Math.floor(Math.random() * (SCREEN_H + 1));
		var spawnX = Math.floor(Math.random() * (SCREEN_W + 1));

		console.log("spawnX: "+this.game.rnd.integerInRange(0, this.SCREEN_W) + ", spawnY: "+this.game.rnd.integerInRange(0, this.SCREEN_W));
		
		console.log("spawnX: "+spawnX + ", spawnY: "+spawnY);
		
		myUnitArray[myUnitCount] = this.game.add.sprite(spawnX, spawnY, 'testUnitImg');
		
		myUnitArray[myUnitCount].animations.add('walk', [0, 1, 2, 3], 10, true);

		myUnitCount++;
		console.log("spawned my unit");
		//myUnitGroup.create(spawnX, spawnY, 'unit');
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

	spawnMine: function() {
		/*
		var m = mineGroup.create(this.rnd.integerInRange(0, 640-158), 960, 'mine');
		this.physics.enable(m, Phaser.Physics.ARCADE);
		m.body.velocity.y = -200*speed+this.rnd.integerInRange(0, 100);
		m.body.velocity.x = this.rnd.integerInRange(-100, 100);
		m.cachedVelocity = {};
		*/
	},
	killUnit: function(unit) {
		/*
		unitGroup
		starsText.setText(starsCount);
		star.kill();
		*/
	},

	restartGame: function() {
		/*
		this.add.tween(gameOverText).to( { alpha: 0 }, 300, Phaser.Easing.Linear.None, true, 0, 0, true);
		this.add.tween(buttonBack).to({x: -160}, 1000, Phaser.Easing.Elastic.Out, true, 0, false);
		var restartTween = this.add.tween(buttonRestart);
		restartTween.to({ x: 640 }, 1000, Phaser.Easing.Elastic.Out);
		restartTween.onComplete.addOnce(function(){
			this.state.start('Game');
		}, this);
		restartTween.start();
		*/
	},

	parallax: function() {
		/*
		bgSeaweed.y -= (speed/2);
		bgAnchors.y -= (speed/4);
		bgMines.y -= (speed/6);
		// if(seaweed.y == -2880) seaweed.y = 0;
		if(this.bgBubbles) {
			this.bgBubbles.y -= speed*4;
		}
		*/
	},

	render: function() {

	}
};