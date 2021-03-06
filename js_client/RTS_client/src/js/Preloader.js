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
		this.load.image('aButtonImg', '../assets/UIbuttons/A-Button.png');
		this.load.image('bButtonImg', '../assets/UIbuttons/B-Button.png');
		this.load.image('eButtonImg', '../assets/UIbuttons/Exl-Button.png');

		this.load.image('metalButton32', '../assets/UIbuttons/metalButton_32x32.png');
		this.load.image('metalButton64', '../assets/UIbuttons/metalButton_64x64.png');		
	},
	create: function() {
		console.log("In preloader, heading to Main Menu");
		this.game.state.start('MainMenu');
	}
};