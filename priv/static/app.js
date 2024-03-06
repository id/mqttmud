if (localStorage.getItem('username') && localStorage.getItem('password')) {
  document.getElementById('username').value = localStorage.getItem('username');
  document.getElementById('password').value = localStorage.getItem('password');
}

const togglePassword = document.querySelector("#togglePassword");
togglePassword.addEventListener("click", function () {
  const password = document.querySelector("#password");
  if (password.getAttribute("type") === "password") {
    password.setAttribute("type", "text");
    this.classList.toggle("bi-eye")
    this.classList.toggle("bi-eye-slash")
  } else {
    password.setAttribute("type", "password");
    this.classList.toggle("bi-eye")
    this.classList.toggle("bi-eye-slash");
  }
});

function generatePassword() {
  const password = Math.random().toString(36).slice(-8);
  document.getElementById('password').value = password;
  document.getElementById('password').disabled = !document.getElementById('password').disabled;
}

function register() {
  const username = document.getElementById('username').value;
  if (username.includes(' ')) {
    document.getElementById('alertText').innerHTML = 'Username cannot contain spaces.';
    document.getElementById('alert').style.visibility = 'visible';
    return;
  }
  if (username.length < 2) {
    document.getElementById('alertText').innerHTML = 'Min username length is 2 characters.';
    document.getElementById('alert').style.visibility = 'visible';
    return;
  }
  if (username.length > 128) {
    document.getElementById('alertText').innerHTML = 'Max username length is 128 characters.';
    document.getElementById('alert').style.visibility = 'visible';
    return;
  }
  const password = document.getElementById('password').value;
  if (password.length > 128) {
    document.getElementById('alertText').innerHTML = 'Max password length is 128 characters.';
    document.getElementById('alert').style.visibility = 'visible';
    return;
  }
  if (password.length < 2) {
    document.getElementById('alertText').innerHTML = 'Min password length is 2 characters.';
    document.getElementById('alert').style.visibility = 'visible';
    return;
  }
  
  fetch('/api/v1/users', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ username: username, password: password }),
  })
    .then(data => {
      if (data.ok) {
      console.log('Registration successful', data);
        localStorage.setItem('username', username);
        localStorage.setItem('password', password);

        window.location.href = 'game.html';
      } else {
        document.getElementById('alertText').innerHTML = 'Registration failed. Please try again.';
        document.getElementById('alert').style.visibility = 'visible';
        console.error('Registration failed:', data);
      }
    })
    .catch((error) => {
      console.error('Error:', error);
    });
}

function login() {
  const username = document.getElementById('username').value;
  const password = document.getElementById('password').value;
  const clientid = username + Math.random().toString(36).substring(7);
  
  const client = mqtt.connect('ws://' + location.hostname + ':8083/mqtt', {
    username: username,
    password: password,
    clientId: clientid,
    resubscribe: false,
    clean: false,
  });
  client.on('connect', function () {
    console.log('Logged in successfully.');
    client.end();
    localStorage.setItem('username', username);
    localStorage.setItem('password', password);
    localStorage.setItem('clientid', clientid);

    window.location.href = 'game.html';
  });
  client.on('error', function (error) {
    console.error('Login failed:', error);
    client.end();
    document.getElementById('alertText').innerHTML = 'Login failed. Please try again.';
    document.getElementById('alert').style.visibility = 'visible';
  });
}
