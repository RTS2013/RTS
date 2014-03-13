//stupid temp objs we will get rid of when the server is all hooked up
var obj1 = {unitType:0,x:10,y:10,z:0,newFacing:0};
var obj2 = {unitType:1,x:20,y:20,z:0,newFacing:0};


//arrays for all your, umm...  all the static stuff?
var models     = new Array();
var bitmaps    = new Array();
var sounds     = new Array();

var modelURLs  = new Array();
var bitmapURLs = new Array();
var soundURLs  = new Array();

modelURLs.push('models/TestFemale.dae');
modelURLs.push('models/ColladaMale.dae');

//create sounds and bitmap URLs here

loadAssets = function(){
    loadEntities();
    loadSound(1);
    loadBitmaps(1);
    
    console.log("calling spawnEntity");

    //spawnEntity(obj1);
    //spawnEntity(obj2);

    console.log("loadAssets working");

}

//create units and shove them in the array so we can clone them later.
//we clone them out of this array with spawnEntity
loadEntities = function(){
    for(var i = 0; i < modelURLs.length; i++){	
    	var loader = new THREE.ColladaLoader();
    	loader.options.convertUpAxis = true;
    	
        loader.load(modelURLs[i], function (collada) {

          	var baseModel = collada.scene;
            baseModel.updateMatrix();

            //var object = objectProto.clone();
            //intead of cloning here we now clone in spawnEntity

    		//object.rotation.z = Math.PI/180;
        
            //scale the models before we put them in the array
            baseModel.scale.x = 10;
            baseModel.scale.y = 10;
            baseModel.scale.z = 10;

            

            models.push(baseModel);
            console.log("loadEnitities working");
            scene.add(models[i]);

        });
    }
		
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