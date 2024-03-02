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
  const password = document.getElementById('password').value;
  
  localStorage.setItem('username', username);
  localStorage.setItem('password', password);

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
  
  const client = mqtt.connect('wss://' + location.hostname + ':8084/mqtt', {
    username: username,
    password: password,
    clientId: username,
    resubscribe: false,
    clean: false,
  });
  client.on('connect', function () {
    console.log('Logged in successfully.');
    client.end();
    localStorage.setItem('username', username);
    localStorage.setItem('password', password);

    window.location.href = 'game.html';
  });
  client.on('error', function (error) {
    console.error('Login failed:', error);
    client.end();
    document.getElementById('alertText').innerHTML = 'Login failed. Please try again.';
    document.getElementById('alert').style.visibility = 'visible';
  });
}
