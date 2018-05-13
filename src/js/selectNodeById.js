$( document ).ready(function() {



  function manageSelectById(e) {

    var network=window.network
    //var nodeId=window.nodeSelecthtmlwidgetSelected;
    nodeId=e;
    console.log('node selected by id: '+nodeId);

    var pos = network.getPositions([nodeId]);
    var currentScale = network.getScale();
    var targetScale = (currentScale>1.2?currentScale:1.2);
    network.moveTo({
      position: {x:pos[nodeId].x, y:pos[nodeId].y},
      scale: targetScale,
      animation: {
        duration:2000,
        easingFunction:'easeInOutCubic'
      }
    });//moveTo

    nodes=network.body.data.nodes;
    edges=network.body.data.edges;
    network.selectNodes([nodeId],true);

    nodes.forEach(function(node) {
      nodes.update({ id: node.id, color: 'rgba(200,200,200,0.5)' })
    })

    //update selected node:
    nodes._data[nodeId].color=nodes._data[nodeId].bodyHiddenColor;
    nodes._data[nodeId].label=nodes._data[nodeId].hiddenLabel;
    nodes.update(nodes.get(nodeId));

    //update direct edges:
    var selectedEdges=network.getSelectedEdges();
    selectedEdges.forEach(function(edgeId){
      edges._data[edgeId].color={inherit: true};
      edges.update(edges.get(edgeId));
    })

  }

  var selectById = document.querySelector('[id^=\"nodeSelecthtmlwidget-\"]');
  selectById.onchange=function(){manageSelectById(this.value);};

});
