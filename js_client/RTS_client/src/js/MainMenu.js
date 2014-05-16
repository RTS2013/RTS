RTS.MainMenu = function(game) {
	font48px = null;
	buttonStart = null;
	depth = 0;
	starsCount = 0;
};
RTS.MainMenu.prototype = {
	create: function() {
		console.log("In Main menu, heading to Game");
		this.game.state.start('Game');
		/*
		this.add.sprite(0, 0, 'screen-mainmenu');

		highscoreTxt = this.game.add.sprite(640-261-60, 580, 'text-highscore');
		highscoreText = this.game.add.text(450, 665, ""+Math.floor(depth/10)+" m", { font: "48px BigFish", fill: "#FFF", stroke: '#000', strokeThickness: 8 });
		highscoreText.anchor.setTo(0.5,0.5);

		this.add.sprite(75, 630, 'hud-star');
		overallTxt = this.game.add.sprite(60, 580, 'text-overall');
		totalscoreText = this.game.add.text(180, 665, ""+starsCount, { font: "48px BigFish", fill: "#FFF", stroke: '#000', strokeThickness: 8 });
		totalscoreText.anchor.setTo(0.5,0.5);

		buttonStart = this.add.button(640, 960-220-20, 'button-start', this.clickStart, this);
		buttonEnclave = this.add.button(-225, 960-80-20-83-30, 'button-enclave', this.clickEnclave, this);
		buttonMoreGames = this.add.button(-250, 960-83-20, 'button-moregames', this.clickMoreGames, this, 1, 0, 2);

		this.game.add.tween(buttonStart).to({x: 640-220-40}, 1000, Phaser.Easing.Elastic.Out, true, 0, false);
		this.game.add.tween(buttonEnclave).to({x: 40}, 1000, Phaser.Easing.Elastic.Out, true, 100, false);
		this.game.add.tween(buttonMoreGames).to({x: 40}, 1000, Phaser.Easing.Elastic.Out, true, 200, false);

		highscoreText.alpha = 0;
		highscoreTxt.alpha = 0;
		totalscoreText.alpha = 0;
		overallTxt.alpha = 0;
		this.game.add.tween(highscoreText).to( { alpha: 1 }, 500, Phaser.Easing.Linear.None, true, 0, 0, true);
		this.game.add.tween(highscoreTxt).to( { alpha: 1 }, 500, Phaser.Easing.Linear.None, true, 0, 0, true);
		this.game.add.tween(totalscoreText).to( { alpha: 1 }, 500, Phaser.Easing.Linear.None, true, 0, 0, true);
		this.game.add.tween(overallTxt).to( { alpha: 1 }, 500, Phaser.Easing.Linear.None, true, 0, 0, true);
		*/
	},
	clickStart: function() {
		this.game.state.start('Game');
	}, 
	clickEnclave: function() {
		window.top.open('http://enclavegames.com/');
	},
	clickMoreGames: function() {
		window.top.open('http://enclavegames.com/games.html');
	}
};