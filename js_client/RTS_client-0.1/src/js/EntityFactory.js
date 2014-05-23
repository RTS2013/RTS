var EF = {
	madeToDate: 0,
	working: false,
	myStructArray: [],
	//friendStructArray: [],
	enemyStructArray: [],
	neutralScructArray: [],

	//for ambient wind blowing and rats scurrying;  pretty much just sprite terrain
	passiveSpriteArray: [],
	
	//Unit groups
	myUnitArray: [],
	myUnitCount: 0,

	enemyUnitArray: [],
	enemyUnitCount: 0,

	//friendUnitArray: [],
	//friendUnitCount: 0,

	spawnMyUnit: function() {
		//these functions actually need to take spawn points but whatever for now


		var spawnY = Math.floor(Math.random() * (SCREEN_H + 1));
		var spawnX = Math.floor(Math.random() * (SCREEN_W + 1));

		//console.log("spawnX: "+this.game.rnd.integerInRange(0, game.SCREEN_W) + ", spawnY: "+this.game.rnd.integerInRange(0, game.SCREEN_W));
		//console.log("SCREEN_W: "+SCREEN_W + ", SCREEN_H: "+SCREEN_H);
		console.log("spawnX: "+spawnX + ", spawnY: "+spawnY);
		
		this.myUnitArray[this.myUnitCount] = game.add.sprite(spawnX, spawnY, 'testUnitImg');
		
		this.myUnitArray[this.myUnitCount].animations.add('walk', [0, 1, 2, 3], 10, true);

		this.myUnitCount++;
		console.log("spawned my unit");
		//myUnitGroup.create(spawnX, spawnY, 'unit');
	},

	spawnEnemyUnit: function() {
		//these functions actually need to take spawn points but whatever for now

		var spawnY = Math.floor(Math.random() * (SCREEN_H + 1));
		var spawnX = Math.floor(Math.random() * (SCREEN_W + 1));
		console.log("spawnX: "+spawnX + ", spawnY: "+spawnY);

		this.enemyUnitArray[this.enemyUnitCount] = game.add.sprite(spawnX, spawnY, 'testUnitImg');

		this.enemyUnitArray[this.enemyUnitCount].animations.add('walk', [0, 1, 2, 3], 10, true);

		//setting this.enemy angle to check for degs or rads.  Its DEGREES.
		this.enemyUnitArray[this.enemyUnitCount].angle = 180; 

		this.enemyUnitCount++;
		//console.log("spawned enemy unit: #"+this.enemyUnitCount);
		//myUnitGroup.create(spawnX, spawnY, 'testUnit');
	}//,

};