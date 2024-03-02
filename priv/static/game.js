const client = mqtt.connect('ws://' + location.hostname + ':8083/mqtt', {
  username: localStorage.getItem('username'),
  password: localStorage.getItem('password'),
  clientId: localStorage.getItem('username'),
  resubscribe: false,
  clean: false,
  properties: {
    sessionExpiryInterval: 86400
  },
});
client.on('connect', function () {
  console.log('Connected to MQTT Broker');
});
client.on('message', function (topic, message) {
  parsedMessage = JSON.parse(message.toString());
  switch (parsedMessage.type) {
  case 'message':
    displayMessage(parsedMessage.from, parsedMessage.message, '.text-info');
    break;
  case 'notification':
    displayMessage(parsedMessage.from, parsedMessage.message, '.text-secondary');
    break;
  case 'voice':
    displayMessage(parsedMessage.from, parsedMessage.message, '.text-primary');
    break;
  case 'command':
    handleCommand(parsedMessage);
    break;
  case 'data':
    handleData(parsedMessage);
    break;
  }
});
client.on('subscribe', function (topic) {
  console.log('Subscribed to', topic);
});

function handleCommand(parsedMessage) {
  const msg = parsedMessage.message;
  switch (msg.command) {
  case 'move':
    displayMessage(parsedMessage.from, 'You entered ' + msg.message + '.', '.text-info');
    document.getElementById('room').innerHTML = msg.message;
    break;
  }
}

function handleData(parsedMessage) {
  const msg = parsedMessage.message;
  switch (msg.dataType) {
  case 'look':
    playersString = msg.players.length > 0 ? msg.players.join(', ') : 'no one else';
    exitsString = msg.exits.join(', ');
    displayMessage(parsedMessage.from, 'You are in a room with ' + playersString + '. Exits are: ' + exitsString + '.', '.text-info');
    break;
  }
}

function sendMessage() {
  const message = document.getElementById('messageInput').value;
  if (message) {
    client.publish('game', message);
    displayMessage(localStorage.getItem('username'), message, '.text-primary');
    document.getElementById('messageInput').value = '';
  }
}

function displayMessage(from, message, style) {
  const chatHistory = document.getElementById('chat-history');
  const fromElement = document.createElement('div');
  fromElement.classList.add('sender');
  fromElement.textContent = from;
  const messageElement = document.createElement('div');
  messageElement.appendChild(fromElement);
  messageElement.innerHTML += message;
  messageElement.classList.add('chat-message');
  if (from === localStorage.getItem('username')) {
    messageElement.classList.add('user-message');
  } else {
    messageElement.classList.add('response-message');
  }
  messageElement.classList.add(style);
  chatHistory.appendChild(messageElement);
  chatHistory.scrollTop = chatHistory.scrollHeight;
}

function leave() {
  client.end();
  localStorage.removeItem('username');
  localStorage.removeItem('password');
  window.location.href = 'index.html';
}
