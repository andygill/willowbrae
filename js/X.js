// Wrapper for the THREE.js library
var X = function(val) {
    var me =  {_this: val}
    me.map = function (t) { 
        console.log("me.map")
        return val.map = t
    }
    return me
}

// Should be called 
var surfaceCache = {};

function surface(args) {
  console.log(args,surfaceCache)
  var src = args.src || "";
  if (surfaceCache[src]) {
    var floorMaterial = surfaceCache[src];
  } else {
  	var floorTexture = new THREE.ImageUtils.loadTexture( src );
  	var floorMaterial = new THREE.MeshBasicMaterial( { 
        map: floorTexture,     /*  */
        side: THREE.DoubleSide /* both sides of this mesh are drawn */ 
    });
    surfaceCache[src] = floorMaterial;
  }
  var size = args.size || new THREE.Vector2(100,100)
	var floorGeometry = new THREE.PlaneGeometry(size.x,size.y);
	var floor = new THREE.Mesh(floorGeometry, floorMaterial);
  return floor;
}
