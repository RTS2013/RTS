define(['model'], function(model){
    var c = canvas.getContext('webGL');
    
    function updateView(){
        model.each(renderEntity);
        
        renderer.render(scene, camera);
        // request new frame
        requestAnimationFrame(function(){
            updateView();
        });
    }

    function renderEntity(entity){
        var x = entity.get('x'),
            y = entity.get('y'),
            z = entity.get('z');

        entity.drawMe(x, y, z);
    }

    model.on('add', function(){
        console.log('view model add');
    });
});
