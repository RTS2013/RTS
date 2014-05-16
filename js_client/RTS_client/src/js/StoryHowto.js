RTS.StoryHowto = function(game) {
	buttonContinue = null;
	state = null;
	font24px = null;
};
RTS.StoryHowto.prototype = {
	create: function() {
		this.showStory();
	},
	showStory: function() {
		/*
		this.game.add.sprite(0, 0, 'screen-story');
		this.buttonContinue = this.add.button(640, 960-220-20, 'button-start', this.showHowto, this, 1, 0, 2);
		this.game.add.tween(this.buttonContinue).to({x: 640-220-20}, 1000, Phaser.Easing.Elastic.Out, true, 0, false);
		*/
		console.log("Howto screen loaded");
		this.game.state.start('Game');
	},
	showHowto: function() {
		/*
		this.game.add.sprite(0, 0, 'screen-howto');
		this.buttonContinue = this.add.button(640, 960-220-20, 'button-start', this.startGame, this, 1, 0, 2);
		this.game.add.tween(this.buttonContinue).to({x: 640-220-20}, 1000, Phaser.Easing.Elastic.Out, true, 0, false);
		*/
	},
	startGame: function() {
		this.game.state.start('Game');
	}
};