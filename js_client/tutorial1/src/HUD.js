HUD = function(game) {
	this.game = game;
	this.score = 0;
	this.scoreText = null;
	this.addButton = null;
	this.subButton = null;
	this.controlGroupButtons = [];
};

HUD.prototype = {
	preload: function(){
		this.game.load.spritesheet('button', '../assets/buttons/button_sprite_sheet.png', 193, 71);
		this.game.load.spritesheet('buttonfire', '../assets/buttons/buttons-big/button-round-a.png',96,96);
    	this.game.load.spritesheet('buttonjump', '../assets/buttons/buttons-big/button-round-b.png',96,96);
	},

	create: function() {
		this.scoreText = this.game.add.text(16, 16, 'score: 0', { fontSize: '32px', fill: '#0f0' });
		
		this.addButton = this.game.add.button(95, 400, 'button', this.actionOnClick, this, 2, 1, 0);
		this.addButton.anchor.setTo(0.01, 0.9);
		this.addButton.angle = 45;
		this.subButton = this.game.add.button(50, 50, 'button', this.actionOnClick, this, 2, 1, 0);
		this.subButton.anchor.setTo(0.9, 0.01);
		//this.buttons[0] = this.game.add.button(95, 400, 'button', actionOnClick, this, 2, 1, 0);

		this.controlGroupButtons = this.game.add.group();
	},
	update: function(){
		//Don't ned this yet but I might
	},
	actionOnClick: function() {
    	console.log("onclicked working");
	}
};