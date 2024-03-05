const client = mqtt.connect('ws://' + location.hostname + ':8083/mqtt', {
  username: localStorage.getItem('username'),
  password: localStorage.getItem('password'),
  clientId: localStorage.getItem('clientid'),
  resubscribe: false,
  clean: false,
  properties: {
    sessionExpiryInterval: 3600
  },
});
client.on('connect', function () {
  console.log('Connected to MQTT Broker');
});
client.on('message', function (topic, message) {
  parsedMessage = JSON.parse(message.toString());
  if (topic === 'users/' + localStorage.getItem('username') + '/fight') {
    if (parsedMessage.message === 'on') {
      document.getElementById('chat-history').classList.add('bg-danger-subtle');
      document.getElementById('chat-history').classList.remove('bg-body');
      return;
    } else if (parsedMessage.message === 'off') {
      document.getElementById('chat-history').classList.add('bg-body');
      document.getElementById('chat-history').classList.remove('bg-danger-subtle');
      return;
    }
  }
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
  console.log(msg);
  switch (msg.dataType) {
  case 'look':
    playersString = msg.players.length > 0 ? msg.players.join(', ') : 'no one else';
    exitsString = msg.exits.length > 0 ? '. Exits are to ' + msg.exits.join(', ') : ' and no exits.';
    monstersString = msg.monsters.length > 0 ? ' You see a ' + msg.monsters.join(', ') + '!' : '';
    displayMessage(parsedMessage.from, 'You are in a room with ' + playersString + exitsString + '.' + monstersString, 'text-success');
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
    displayMessage(parsedMessage.from, msg.message, '', 'bi-volume-up');
    break;
  case 'whisper':
    displayMessage(parsedMessage.from, msg.message, 'fst-italic', 'bi-lock');
    break;
  case 'shout':
    displayMessage(parsedMessage.from, msg.message, 'font-weight-bold,text-uppercase', 'bi-megaphone');
    break;
  }
}

function sendMessage() {
  const message = document.getElementById('messageInput').value;
  if (message) {
    client.publish('game', message);
    displayMessage(localStorage.getItem('username'), message);
    document.getElementById('messageInput').value = '';
  }
}

function displayMessage(from, message, styles = '', icon = '') {
  const chatHistory = document.getElementById('chat-history');
  const fromElement = document.createElement('div');
  fromElement.classList.add('sender');
  fromElement.textContent = from;
  const messageText = document.createElement('p');
  if (icon) {
    const iconElement = document.createElement('i');
    iconElement.classList.add('bi');
    iconElement.classList.add(icon);
    iconElement.innerHTML = '&nbsp;&nbsp;';
    messageText.appendChild(iconElement);
  }
  messageText.innerHTML += message;
  messageText.classList.add('chat-message');
  if (from === localStorage.getItem('username')) {
    fromElement.classList.add('self');
    messageText.classList.add('self');
    messageText.classList.add('bg-primary');
    messageText.classList.add('text-white');
  } else if (from !== 'DM') {
    fromElement.classList.add('other');
    messageText.classList.add('other');
    messageText.classList.add('bg-success');
    messageText.classList.add('text-white');
  } else {
    fromElement.classList.add('other');
    messageText.classList.add('other');
    messageText.classList.add('bg-light');
  }
  if (styles) {
    styles.split(',').forEach(style =>
      messageText.classList.add(style)
    );
  }
  const messageElement = document.createElement('div');
  messageElement.appendChild(fromElement);
  messageElement.appendChild(messageText);
  chatHistory.appendChild(messageElement);
  chatHistory.scrollTop = chatHistory.scrollHeight;
}

function leave() {
  client.end();
  window.location.href = 'index.html';
}
