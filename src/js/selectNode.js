function(e) {
  var nodeId = e.nodes[0];
  var pos = this.getPositions([nodeId]);
  this.moveTo({position: {x:pos[nodeId].x, y:pos[nodeId].y}});
}//selectNode
