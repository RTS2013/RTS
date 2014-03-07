
//model array for all your, umm...  models
var modelArray = new Array();
var urls = new Array();

urls.push('models/TestFemale.dae');
urls.push('models/ColladaMale.dae');


//create units and shove them in the array so we can clone them later
loadEntities = function(){
    for(var i = 0; i < urls.length; i++){	
    	loader = new THREE.ColladaLoader();
    	loader.options.convertUpAxis = true;
    	loader.load(urls[i], function (collada) {

      	var baseModel = collada.scene;
        //var object = objectProto.clone();
        //intead of cloning here we now clone in spawnEntity

    		//object.rotation.z = Math.PI/180;
        
        //scale the models before we put them in the array
        object.scale.x = 10;
        object.scale.y = 10;
        object.scale.z = 10;

        modelArray.push(baseModel);

      }
  	});
		
}