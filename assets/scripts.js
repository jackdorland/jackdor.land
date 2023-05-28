addEventListener('load', () => {
  const date = new Date();

  document.getElementById('date').innerText = date.toLocaleString('default', {
    dateStyle: 'long'
  });
})
