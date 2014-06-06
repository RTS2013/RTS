 var EF = {
	madeToDate: 0,
	working: false,

	////////////////
	//Containers
	////////////////

	//Structure groups
	myStructArray: [], enemyStructArray: [], neutralScructArray: [],

	//Unit groups
	myUnitArray: [], myUnitCount: 0,

	enemyUnitArray: [], enemyUnitCount: 0,

	neutralUnitArray: [], neutralUnitCount: 0,

	//friendUnitArray: [], friendUnitCount: 0,

	//for ambient wind blowing and rats scurrying;  pretty much just sprite terrain
	passiveSpriteArray: [], passiveSpriteCount: 0,

	listen: function(){
		//animate units
		for(var i = 0; i < EF.myUnitCount; i++){
			switch (EF.myUnitArray[i].doing) {
			case 'wait':
			    EF.myUnitArray[i].sprite.animations.play('wait');
			    break;
			case 'fire':
			    EF.myUnitArray[i].sprite.animations.play('fire');
			    break;
			case 'melee':
			    EF.myUnitArray[i].sprite.animations.play('melee');
			    break;
			case 'scrounge':
			    EF.myUnitArray[i].sprite.animations.play('scrounge');
			    break;
			case 'walk':
			    EF.myUnitArray[i].sprite.animations.play('walk');
			    break;
			case 'die':
			    EF.myUnitArray[i].sprite.animations.play('die');
			    break;
			case 'magic':
			    EF.myUnitArray[i].sprite.anamations.play('magic');
			    break;
			}

		}

		for(var i = 0; i < EF.enemyUnitCount; i++){
			switch (EF.enemyUnitArray[i].doing) {
			case 'wait':
			    EF.enemyUnitArray[i].sprite.animations.play('wait');
			    break;
			case 'fire':
			    EF.enemyUnitArray[i].sprite.animations.play('fire');
			    break;
			case 'melee':
			    EF.enemyUnitArray[i].sprite.animations.play('melee');
			    break;
			case 'scrounge':
			    EF.enemyUnitArray[i].sprite.animations.play('scrounge');
			    break;
			case 'walk':
			    EF.enemyUnitArray[i].sprite.animations.play('walk');
			    break;
			case 'die':
			    EF.enemyUnitArray[i].sprite.animations.play('die');
			    break;
			case 'magic':
			    EF.enemyUnitArray[i].sprite.anamations.play('magic');
			    break;
			}
		}
		/*
		for(var i = 0; i < EF.myUnitCount; i++){
			EF.myUnitArray[i].animations.play('walk');
		}
		for(var i = 0; i < EF.enemyUnitCount; i++){
			EF.enemyUnitArray[i].animations.play('walk');
		}
		*/
	},

	spawnMyUnit: function() {
		//these functions actually need to take spawn points but whatever for now


		var spawnY = Math.floor(Math.random() * (SCREEN_H + 1));
		var spawnX = Math.floor(Math.random() * (SCREEN_W + 1));
		console.log("spawnX: "+spawnX + ", spawnY: "+spawnY);
		
		//this.myUnitArray[this.myUnitCount] = game.add.sprite(spawnX, spawnY, 'testUnitImg');
		this.myUnitArray[this.myUnitCount] = new Unit(spawnX, spawnY, 0,'animUnitSheet');
		this.myUnitArray[this.myUnitCount].init();

		//this.myUnitArray[this.myUnitCount].sprite.animations.add('walk', [0, 1, 2, 3], 10, true);

		this.myUnitArray[this.myUnitCount].sprite.animations.add('fire', 	 	[ 0, 1, 2, 3], 10, true);
		this.myUnitArray[this.myUnitCount].sprite.animations.add('wait', 	 	[ 4, 5, 6, 7], 10, true);
		this.myUnitArray[this.myUnitCount].sprite.animations.add('melee',  		[ 8, 9,10,11], 10, true);
		this.myUnitArray[this.myUnitCount].sprite.animations.add('scrounge',	[12,13,14,15], 10, true);
		this.myUnitArray[this.myUnitCount].sprite.animations.add('walk', 	 	[16,17,18,19], 10, true);
		this.myUnitArray[this.myUnitCount].sprite.animations.add('die', 	 	[20,21,22,23], 10, true);
		this.myUnitArray[this.myUnitCount].sprite.animations.add('magic', 		[24,25,26,27], 10, true);

		this.myUnitCount++;
		//console.log(HUD.unitText + "");
    	unitText.content = ('Units: ' + this.myUnitCount);
		console.log("spawned my unit");
		//myUnitGroup.create(spawnX, spawnY, 'unit');
	},

	spawnEnemyUnit: function() {
		//these functions actually need to take spawn points but whatever for now

		var spawnY = Math.floor(Math.random() * (SCREEN_H + 1));
		var spawnX = Math.floor(Math.random() * (SCREEN_W + 1));

		//this.enemyUnitArray[this.enemyUnitCount] = game.add.sprite(spawnX, spawnY, 'testUnitImg');
		this.enemyUnitArray[this.enemyUnitCount] = new Unit(spawnX, spawnY, 0,'animUnitSheet');
		this.enemyUnitArray[this.enemyUnitCount].init();

		//this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('walk', 	[0, 1, 2, 3], 10, true);

		this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('fire', 		[ 0, 1, 2, 3], 10, true);
		this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('scrounge', 	[ 4, 5, 6, 7], 10, true);
		this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('walk',  	[ 8, 9,10,11], 10, true);
		this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('wait',		[12,13,14,15], 10, true);
		this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('die', 		[16,17,18,19], 10, true);
		this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('magic', 	[20,21,22,23], 10, true);
		this.enemyUnitArray[this.enemyUnitCount].sprite.animations.add('melee', 	[24,25,26,27], 10, true);


		//setting this.enemy angle to check for degs or rads.  Its DEGREES.
		//this.enemyUnitArray[this.enemyUnitCount].sprite.angle = 180; 

		this.enemyUnitCount++;
		enemyText.content = ('Enemies: ' + this.enemyUnitCount);
		console.log("spawned enemy unit: #"+this.enemyUnitCount);
		//myUnitGroup.create(spawnX, spawnY, 'testUnit');
	}//,

};