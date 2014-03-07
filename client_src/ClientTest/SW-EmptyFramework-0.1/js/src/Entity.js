define([], function(){
    return Backbone.Model.extend({
        x: 10,
        y: 10,
        z: 10,
    
        hp:     100,
        armor:  0,
        speed:  2.5,
        
        fear:   0.5
    });
});
