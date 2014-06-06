var RTS = {};
RTS.Boot = function(game){};
RTS.Boot.prototype = {
	preload: function(){
		//assets like loading bar pics go here
		//this.load.image('preloaderBar', 'img/loading-bar.png');
		//this.load.image('submarine', 'img/submarine.png');
	},
	create: function(){
		//Set device settings before loading level settings
		/*
		this.game.input.maxPointers = 1;
		this.game.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
		if(!this.game.device.desktop) {
			this.game.scale.forcePortrait = true;
		}
		this.game.scale.pageAlignHorizontally = true;
		this.game.scale.pageAlignVertically = true;
		this.game.scale.setScreenSize(true);
		*/

		//move to the next state
		console.log("In Boot, heading to preloader");
		this.game.state.start('Preloader');
	}
};