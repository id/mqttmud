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

  fetch('http://localhost:8080/api/v1/users', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ username: username, password: password }),
  })
    .then(data => {
      console.log('Registration successful', data);
      window.location.href = 'game.html';
    })
    .catch((error) => {
      console.error('Error:', error);
    });
}

function login() {
  const username = document.getElementById('username').value;
  const password = document.getElementById('password').value;
  
  localStorage.setItem('username', username);
  localStorage.setItem('password', password);

  window.location.href = 'game.html';
}
