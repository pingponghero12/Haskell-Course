document.getElementById('dataForm').addEventListener('submit', function (e) {
    e.preventDefault(); // Prevent the form from submitting normally
    const input = document.getElementById('dataInput').value;
    alert('You entered: ' + input);
});
