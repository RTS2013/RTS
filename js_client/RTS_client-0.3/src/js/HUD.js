var HUD = {
	//HUD VARIABLES
	
	displayText: "none",
	coinText: "default",

	//UI buttons

	//action (left hand)
	aButton: null,
	bButton: null,
	eButton: null,

	//control groups
	ctrlGroupBtn1: null, ctrlGroupBtn2: null, ctrlGroupBtn3: null, ctrlGroupBtn4: null, ctrlGroupBtn5 : null,
	ctrlGroupBtn6: null, ctrlGroupBtn7: null, ctrlGroupBtn8: null, ctrlGroupBtn9: null, ctrlGroupBtn10: null,

	//mini map
	miniMap: null,
	
	createHUD: function() {
		// HUD
    	game.input.addPointer();

    	game.add.text(10, 10, "COIN: 0", { font: "32px Arial", fill: "#330088", align: "center" });

    	//Left Hand buttons

	    aButton = game.add.button(10, SCREEN_H - 74, 'metalButton64', this.actionOnClickA, game, 2, 1, 0);
    	aButton.inputEnabled = true;
    	aButton.fixedToCamera = true;
    	
    	bButton = game.add.button(10 + 64 + 10, SCREEN_H - 74, 'metalButton64', this.actionOnClickB, game, 2, 1, 0);
    	bButton.inputEnabled = true;
    	bButton.fixedToCamera = true;
    	
    	eButton = game.add.button(10 + 128 + 20, SCREEN_H - 74, 'metalButton64', this.actionOnClickE, game, 2, 1, 0);
    	eButton.inputEnabled = true;
    	eButton.fixedToCamera = true;

    	//Control Group Buttons

    	ctrlGroupBtn1 = game.add.button(SCREEN_W - 320 - 100, 10, 'metalButton32', this.selectCG1, this, 2, 1, 0);
		ctrlGroupBtn1.inputEnabled = true;
    	ctrlGroupBtn1.fixedToCamera = true;

    	ctrlGroupBtn2 = game.add.button(SCREEN_W - 288 - 90, 10, 'metalButton32', this.selectCG2, this, 2, 1, 0);
		ctrlGroupBtn2.inputEnabled = true;
    	ctrlGroupBtn2.fixedToCamera = true;
    	
    	ctrlGroupBtn3 = game.add.button(SCREEN_W - 256 - 80, 10, 'metalButton32', this.selectCG3, this, 2, 1, 0);
		ctrlGroupBtn3.inputEnabled = true;
    	ctrlGroupBtn3.fixedToCamera = true;
    	    	
    	ctrlGroupBtn4 = game.add.button(SCREEN_W - 224 - 70, 10, 'metalButton32', this.selectCG4, this, 2, 1, 0);
		ctrlGroupBtn4.inputEnabled = true;
    	ctrlGroupBtn4.fixedToCamera = true;
    	    	
    	ctrlGroupBtn5 = game.add.button(SCREEN_W -192 - 60, 10, 'metalButton32', this.selectCG5, this, 2, 1, 0);
		ctrlGroupBtn5.inputEnabled = true;
    	ctrlGroupBtn5.fixedToCamera = true;
    	    	
    	ctrlGroupBtn6 = game.add.button(SCREEN_W - 160 - 50, 10, 'metalButton32', this.selectCG6, this, 2, 1, 0);
		ctrlGroupBtn6.inputEnabled = true;
    	ctrlGroupBtn6.fixedToCamera = true;
    	    	
    	ctrlGroupBtn7 = game.add.button(SCREEN_W - 128 - 40, 10, 'metalButton32', this.selectCG7, this, 2, 1, 0);
		ctrlGroupBtn7.inputEnabled = true;
    	ctrlGroupBtn7.fixedToCamera = true;
    	    	
    	ctrlGroupBtn8 = game.add.button(SCREEN_W - 96 - 30, 10, 'metalButton32', this.selectCG8, this, 2, 1, 0);
		ctrlGroupBtn8.inputEnabled = true;
    	ctrlGroupBtn8.fixedToCamera = true;
		
    	ctrlGroupBtn9 = game.add.button(SCREEN_W - 64 - 20, 10, 'metalButton32', this.selectCG9, this, 2, 1, 0);
		ctrlGroupBtn9.inputEnabled = true;
    	ctrlGroupBtn9.fixedToCamera = true;

    	ctrlGroupBtn10 = game.add.button(SCREEN_W - 32 - 10, 10, 'metalButton32', this.selectCG10, this, 2, 1, 0);
		ctrlGroupBtn10.inputEnabled = true;
    	ctrlGroupBtn10.fixedToCamera = true;

		console.log("HUD is working ");
	},

	notOverButtons: function(x, y){
		this.x = x;
		this.y = y;
		if(this.overAnyButton(this.x, this.y)){
			console.log("not over buttons returned: false");
			return false;
		}
		console.log("not over buttons returned: true");
		return true;
	},

	overAnyButton: function(x, y){
		this.x = x;
		this.y = y;
		console.log("this.x, this.y: " +x+", "+y);
		console.log("this.x, this.y: " +this.x+", "+this.y);
		if( this.overButtonA(this.x, this.y) ||
			this.overButtonB(this.x, this.y) ||
			this.overButtonE(this.x, this.y) ||
			this.overControlGroups(this.x, this.y)){
			
			return true;
		}
		return false;
	},

	overButtonA: function(x, y){
		this.x = x;
		this.y = y;

		if((this.x > 10 && this.x < 74) && (this.y < game.SCREEN_H - 10 && this.y > game.SCREEN_H - 74)){
			console.log("over A button");
			return true;
		}
		return false;
	},

	overButtonB: function(x, y){
		this.x = x;
		this.y = y;
		
		if((this.x > 84 && this.x < 138) && (this.y < game.SCREEN_H - 10 && this.y > game.SCREEN_H - 74)){
			console.log("over B button");
			return true;
		}
		return false;	
	},

	overButtonE: function(x, y){
		this.x = x;
		this.y = y;
		
		if((this.x > 158 && this.x < 202) && (this.y < game.SCREEN_H - 10 && this.y > game.SCREEN_H - 74)){
			console.log("over E button");
			return true;
		}
		return false;
	},

	overControlGroups: function(x, y){
		this.x = x;
		this.y = y;
		if((this.x > game.SCREEN_W - 420) && (this.y > 10 && this.y < 42)){
			console.log("over control groups");
			return true;
		}
		return false;
	},

	actionOnClickA: function(){
		EF.spawnMyUnit();
	},
	actionOnClickB: function(){
		EF.spawnEnemyUnit();

	},
	actionOnClickE: function(){
		IN.ADD_MODE = true;
		console.log("ADD_MODE: "+IN.ADD_MODE);
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
	}//,

};