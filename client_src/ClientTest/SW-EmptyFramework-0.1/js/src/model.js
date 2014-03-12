define(['Entity'], function(Entity){
    var Entities = Backbone.Collection.extend({
        model: Entity
    });
    return new Entities();
});
