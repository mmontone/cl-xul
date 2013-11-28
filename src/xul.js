// Components.utils.import('resource://gre/modules/devtools/dbg-server.jsm');
// if (!DebuggerServer.initialized) {
//   DebuggerServer.init();
//   DebuggerServer.addBrowserActors();
// }
// DebuggerServer.openListener(6000);

var connection = new WebSocket('ws://localhost:9998/echo');

// When the connection is open, send some data to the server
connection.onopen = function () {
  //connection.send('Ping'); // Send the message 'Ping' to the server
  dump('Opening connection');
};

// Log errors
connection.onerror = function (error) {
  alert('Error:' +  error.data);
  dump('WebSocket Error ' + error.data);
  
};

// Log messages from the server
connection.onmessage = function (e) {
  //alert(e.data);
  var update = JSON.parse(e.data);
  switch(update.type) {
      case 'replaceComponent':
         document.getElementById(update.id).innerHTML = update.content;
         break;
      default: alert('Invalid update ' + e.data);
  }
  dump('Server: ' + e.data);
};

connection.onclose = function (e) {
    alert('Websocket closing!!' + e.code + e.reason + e.wasClean);
    dump('Close ' + e.code + ':' + e.reason);
};

function sendMessage(message) {
    //alert ('Sending ' + message);
    connection.send(message);
}
