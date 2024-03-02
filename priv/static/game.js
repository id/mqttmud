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
    displayMessage(parsedMessage.from, parsedMessage.message, 'text-success');
    break;
  case 'notification':
    displayMessage(parsedMessage.from, parsedMessage.message, 'text-muted,fst-italic');
    break;
  case 'voice':
    handleVoice(parsedMessage);
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
    displayMessage(parsedMessage.from, 'You entered ' + msg.message + '.', 'text-success');
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
    displayMessage(parsedMessage.from, 'You are in a room with ' + playersString + '. Exits are: ' + exitsString + '.', 'text-success');
    break;
  }
}

function handleVoice(parsedMessage) {
  const self = localStorage.getItem('username');
  if (parsedMessage.from === self) {
    return;
  }
  const voiceType = parsedMessage.message.voiceType;
  const msg = parsedMessage.message;
  switch (voiceType) {
  case 'say':
    displayMessage('DM', `You hear ${parsedMessage.from} say: "${msg.message}"`, 'text-success');
    break;
  case 'whisper':
    displayMessage('DM', `You hear a whisper from ${parsedMessage.from}: "${msg.message}"`, 'text-success,fw-light,fst-italic');
    break;
  case 'shout':
    displayMessage('DM', `You hear a shout: <b>${msg.message}</b>`, 'font-weight-bold,text-uppercase');
    break;
  }
}

function sendMessage() {
  const message = document.getElementById('messageInput').value;
  if (message) {
    client.publish('game', message);
    displayMessage(localStorage.getItem('username'), message, 'text-primary');
    document.getElementById('messageInput').value = '';
  }
}

function displayMessage(from, message, styles) {
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
  styles.split(',').forEach(style =>
    messageElement.classList.add(style)
  );
  chatHistory.appendChild(messageElement);
  chatHistory.scrollTop = chatHistory.scrollHeight;
}

function leave() {
  client.end();
  localStorage.removeItem('username');
  localStorage.removeItem('password');
  window.location.href = 'index.html';
}
