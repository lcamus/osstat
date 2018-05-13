//handler for hovernode event
function(e) {

  var networkCanvas = document.querySelector('[id^=\"graphhtmlwidget-\"]').getElementsByTagName('canvas')[0];
  networkCanvas.style.cursor = 'pointer';
  var table0 = $('#DataTables_Table_0').DataTable();
  var table1 = $('#DataTables_Table_1').DataTable();
  table0.search(e.node,false,false,false).draw();
  $('#DataTables_Table_0 caption').text('Incoming '+e.node);
  table1.search(e.node,false,false,false).draw();
  $('#DataTables_Table_1 caption').text('Outcoming '+e.node);

}//hovernode
