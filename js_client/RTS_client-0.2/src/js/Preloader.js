RTS.Preloader = function(game) {
	this.background = null;
	this.preloadBar = null;
};
RTS.Preloader.prototype = {
	preload: function() {		
		//////////
		//DO: put a preload bar for the Main Menu
		/////////

		//Load all game assets here.... MAYBE.  For now anyway.
		//When we have multiple texture schemes we'll have to make 
		//this state dynamically load in the different "Game" types.

		//But the different game types can load stuff with their
		//own preloader method so I'm not sure if ALL of the 
		//asset loading needs to be done in here or if it can be
		// broken up in the different game types, like "SnowGame" vs 
		//"DesertGame" vs our current which would be plain "MetalGame" 
		//or whatever.

		//Which will really just be distinct asset sets but could
		//also have different UIs really.  as long as the message
		//passing functions aren't changed it should work great with
		//any layout you can think of.
		
		this.load.spritesheet('testUnitImg', '../assets/units/testUnitSprite.png', 64, 64, 4);
		this.load.spritesheet('animUnitSheet', '../assets/units/fullUnitSpriteSheet2.png', 64, 64, 28);
		this.load.image('aButtonImg', '../assets/UIbuttons/A-Button.png');
		this.load.image('bButtonImg', '../assets/UIbuttons/B-Button.png');
		this.load.image('eButtonImg', '../assets/UIbuttons/Exl-Button.png');

		this.load.image('blueButtonImg', '../assets/UIbuttons/blueButton64.png');
		this.load.image('redButtonImg', '../assets/UIbuttons/redButton64.png');
		this.load.image('greenButtonImg', '../assets/UIbuttons/greenButton64.png');

		this.load.image('metalButton32', '../assets/UIbuttons/metalButton_32x32.png');
		this.load.image('metalButton64', '../assets/UIbuttons/metalButton_64x64.png');		
		this.load.image('seamlessConcreteHUGE', '../assets/map/Seamless_concrete_1600x1600.jpg');
		this.load.image('1stBlueTile64', '../assets/map/brokenBlueTile1.png');

		this.load.image('bevBrownTile64', '../assets/brokenBunkerTile1_64x64.png');
		this.load.image('regBrownTile64', '../assets/brokenBunkerTile2_64x64.png');

		this.load.image('mainMenuBG', '../assets/futureProofPhaserMenu.png');
		this.load.image('startButton', '../assets/Start.png');

		game.load.image('ground_1x1', '../assets/ground_1x1.png');
	},
	create: function() {
		console.log("In preloader, heading to Main Menu");
		this.game.state.start('MainMenu');
	}
};