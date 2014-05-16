RTS.Game = function(game) {
	////////////////////////
	//Basic Game variables
	////////////////////////
	this.game = game;

	SCREEN_W = 800;
	SCREEN_H = 480;

	//HUD VARIABLES
	displayText = null;

	///////////////////////////
	//Level/Terrain variables
	///////////////////////////

	//Maps Vars
	tileMap = null;
	
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
	ctrlGroupBtn1 = null;
	ctrlGroupBtn2 = null;
	ctrlGroupBtn3 = null;
	ctrlGroupBtn4 = null;
	ctrlGroupBtn5 = null;
	ctrlGroupBtn6 = null;
	ctrlGroupBtn7 = null;
	ctrlGroupBtn8 = null;
	ctrlGroupBtn9 = null;
	ctrlGroupBtn10 = null;

	//mini map
	miniMap = null;

};

RTS.Game.prototype = {
	create: function() {
    	this.game.stage.backgroundColor = '#2d2d2d';

		//should be no need to init the plain ARCADE physics, it should be running by default
		//this is why this.game.physics is regiestering as an object.

		//this.game.physics.startSystem(Phaser.Physics.ARCADE);
		console.log(""+this.game.physics);

		this.createHUD();

	},

	update: function() {
		for(var i = 0; i < myUnitCount; i++){
			myUnitArray[i].animations.play('walk');
		}
		for(var i = 0; i < enemyUnitCount; i++){
			enemyUnitArray[i].animations.play('walk');
		}
	},

	createHUD: function() {
		// HUD
		this.game.input.addPointer();
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

    	ctrlGroupBtn1 = this.game.add.button(SCREEN_W/2.2, 10, 'metalButton32', this.selectCG1, this, 2, 1, 0);
		ctrlGroupBtn1.inputEnabled = true;
    	ctrlGroupBtn1.fixedToCamera = true;

    	console.log("screen width/screen height " + SCREEN_W/2 +" , " + SCREEN_H);

    	ctrlGroupBtn2 = this.game.add.button(SCREEN_W/2.2 + 32 + 10, 10, 'metalButton32', this.selectCG2, this, 2, 1, 0);
		ctrlGroupBtn2.inputEnabled = true;
    	ctrlGroupBtn2.fixedToCamera = true;
    	
    	ctrlGroupBtn3 = this.game.add.button(SCREEN_W/2.2 + 64 + 20, 10, 'metalButton32', this.selectCG3, this, 2, 1, 0);
		ctrlGroupBtn3.inputEnabled = true;
    	ctrlGroupBtn3.fixedToCamera = true;
    	    	
    	ctrlGroupBtn4 = this.game.add.button(SCREEN_W/2.2 + 96 + 30, 10, 'metalButton32', this.selectCG4, this, 2, 1, 0);
		ctrlGroupBtn4.inputEnabled = true;
    	ctrlGroupBtn4.fixedToCamera = true;
    	    	
    	ctrlGroupBtn5 = this.game.add.button(SCREEN_W/2.2 + 128 + 40, 10, 'metalButton32', this.selectCG5, this, 2, 1, 0);
		ctrlGroupBtn5.inputEnabled = true;
    	ctrlGroupBtn5.fixedToCamera = true;
    	    	
    	ctrlGroupBtn6 = this.game.add.button(SCREEN_W/2.2 + 160 + 50, 10, 'metalButton32', this.selectCG6, this, 2, 1, 0);
		ctrlGroupBtn6.inputEnabled = true;
    	ctrlGroupBtn6.fixedToCamera = true;
    	    	
    	ctrlGroupBtn7 = this.game.add.button(SCREEN_W/2.2 + 192 + 60, 10, 'metalButton32', this.selectCG7, this, 2, 1, 0);
		ctrlGroupBtn7.inputEnabled = true;
    	ctrlGroupBtn7.fixedToCamera = true;
    	    	
    	ctrlGroupBtn8 = this.game.add.button(SCREEN_W/2.2 + 224 + 70, 10, 'metalButton32', this.selectCG8, this, 2, 1, 0);
		ctrlGroupBtn8.inputEnabled = true;
    	ctrlGroupBtn8.fixedToCamera = true;
		
    	ctrlGroupBtn9 = this.game.add.button(SCREEN_W/2.2 + 256 + 80, 10, 'metalButton32', this.selectCG9, this, 2, 1, 0);
		ctrlGroupBtn9.inputEnabled = true;
    	ctrlGroupBtn9.fixedToCamera = true;

    	ctrlGroupBtn10 = this.game.add.button(SCREEN_W/2.2 + 288 + 90, 10, 'metalButton32', this.selectCG10, this, 2, 1, 0);
		ctrlGroupBtn10.inputEnabled = true;
    	ctrlGroupBtn10.fixedToCamera = true;
		

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

	render: function() {}
};