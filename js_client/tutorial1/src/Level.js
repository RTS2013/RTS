Level = function(game) {

	this.game = game;
	this.background = null;
	this.ground = null;
	this.button = null;
	this.platforms = null;
	this.stars = null;
	this.cursors = null;
};

Level.prototype = {

	preload: function() {
		this.game.load.image('sky', '../assets/sky.png');
    	this.game.load.image('ground', '../assets/platform.png');
    	this.game.load.image('star', '../assets/star.png');

    	this.game.load.tilemap('desert', '../assets/tilemaps/maps/desert.json', null, Phaser.Tilemap.TILED_JSON);
    	this.game.load.image('tiles', '../assets/tilemaps/tiles/tmw_desert_spacing.png');

    	//this.game.scale.scaleMode = new Phaser.ScaleManager.SHOW_ALL;
    	//this.game.scale.fullScreenScaleMode = new Phaser.ScaleManager.EXACT_FIT;
	},

	create: function() {
		if (!game.device.desktop){ 
			game.input.onDown.add(this.gofull, this); 
		}

	    this.game.world.setBounds(0, 0, 2000, 2000);

	    for (var i = 0; i < 150; i++)
	    {
	        this.game.add.sprite(this.game.world.randomX, this.game.world.randomY, 'star');
	    }

	    this.cursors = this.game.input.keyboard.createCursorKeys();

		this.game.input.addPointer();
    	this.game.input.addPointer();

		//onInputOver
		//onInputOut
		//onInputDown
		//onInputUp
		//onDragStart
		//onDragStop

		// add background for this level
		//this.game.add.sprite(0, 0, 'sky');

		//  The platforms group contains the ground and the 2 ledges we can jump on
    	//this.platforms = game.add.group();

    	// Here we create the ground.
	    //this.ground = this.platforms.create(0, game.world.height - 64, 'ground');

	    //  Scale it to fit the width of the game (the original sprite is 400x32 in size)
	    //this.ground.scale.setTo(2, 2);
	 
	    //  This stops it from falling away when you jump on it
	    //this.ground.body.immovable = true;

	    //  Now let's create two ledges
	    //var ledge = this.platforms.create(400, 400, 'ground');
	    //ledge.body.immovable = true;
	 
	    //ledge = this.platforms.create(-150, 250, 'ground');
	    //ledge.body.immovable = true;

	    // create a group for stars
	    this.stars = this.game.add.group();
	 
	    //  Here we'll create 12 of them evenly spaced apart
	    for (var i = 0; i < 12; i++)
	    {
	        //  Create a star inside of the 'stars' group
	        var star = this.stars.create(i * 70, i * 70, 'star');
	 
	        //  Let gravity do its thing
	        //star.body.gravity.y = 6;
	 
	        //  This just gives each star a slightly random bounce value
	        //star.body.bounce.y = 0.7 + Math.random() * 0.2;
	    }
	},

	update: function() {
		this.game.physics.collide(this.stars, this.platforms);
		
		if (this.cursors.up.isDown)
	    {
	        this.game.camera.y -= 4;
	    }
	    else if (this.cursors.down.isDown)
	    {
	        this.game.camera.y += 4;
	    }

	    if (this.cursors.left.isDown)
	    {
	        this.game.camera.x -= 4;
	    }
	    else if (this.cursors.right.isDown)
	    {
	        this.game.camera.x += 4;
	    }
	},
	
	gofull: function() { 
		this.game.scale.startFullScreen(false);
	},



	render: function(){
		//this.game.debug.pointer(this.game.input.mousePointer);
    	//this.game.debug.pointer(this.game.input.pointer1);
    	//this.game.debug.pointer(this.game.input.pointer2);
		this.game.debug.cameraInfo(this.game.camera, 32, 32);
	}

};