//stupid temp objs we will get rid of when the server is all hooked up
var obj1 = {unitType:0,newX:10,newY:10,newZ:0,newFacing:0};
var obj2 = {unitType:1,newX:20,newY:20,newZ:0,newFacing:0};
var obj3 = {unitType:2,newX:30,newY:20,newZ:0,newFacing:0};
var obj4 = {unitType:3,newX:40,newY:20,newZ:0,newFacing:0};
var obj5 = {unitType:4,newX:50,newY:20,newZ:0,newFacing:0};
var obj6 = {unitType:5,newX:60,newY:20,newZ:0,newFacing:0};
var obj7 = {unitType:6,newX:70,newY:20,newZ:0,newFacing:0};
var obj8 = {unitType:7,newX:80,newY:20,newZ:0,newFacing:0};

//arrays for all your, umm...  all the static stuff?
var models     = new Array();
var bitmaps    = new Array();
var sounds     = new Array();

var bitmapURLs = new Array();
var soundURLs  = new Array();

//modelURLs.push('models/TestFemale.dae');
//modelURLs.push('models/ColladaMale.dae');

//create sounds and bitmap URLs here


//create units and shove them in the array so we can clone them later.
//we clone them out of this array with spawnEntity
loadEntities = function(){
    var loader = new THREE.ColladaLoader();
	loader.options.convertUpAxis = true;
	loader.load("models/Everybody.dae", function (collada) {

		var baseModel = collada.scene;
		baseModel.updateMatrix();
		console.log(collada.scene);

		//object.rotation.z = Math.PI/180;
		//scale the models before we put them in the array
		baseModel.scale.x = 10;
		baseModel.scale.y = 10;
		baseModel.scale.z = 10;
	
		baseModel.children.forEach(function(child){
			child.rotation.x = 0;
			child.rotation.y = 0;
			child.rotation.z = 0;
		});
	
		
		//scene.add(baseModel);
		console.log("loadEnitities working");
		//scene.add(baseModel);
		for(var i = 0; i < baseModel.children.length; i++){
			console.log(""+baseModel.children[i]);
			models.push(baseModel.children[i]);
			console.log("in model parser");
		}

		//spawnEntity(obj1);
		//spawnEntity(obj2);
		//spawnEntity(obj3);
		//spawnEntity(obj4);
		//spawnEntity(obj5);
		//spawnEntity(obj6);
		spawnEntity(obj7);
		spawnEntity(obj8);
	});
}

loadBitmaps = function(count){
    //
    for(var i = 0; i < count; i++){
        //create bitmap, then push it
        //bitmaps.push("bitmap_"+i);
    }
}

loadSound = function(count){
    for(var i = 0; i < count; i++){
        //create sound objects here
    }

}
loadAssets = function(){
    loadEntities();
    loadSound(1);
    loadBitmaps(1);
    
    console.log("calling spawnEntity");

    console.log("loadAssets working");

}
