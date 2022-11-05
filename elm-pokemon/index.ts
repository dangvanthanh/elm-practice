import Bao from 'baojs';

const app = new Bao();

app.get('/api/v2', (ctx) => {
  return ctx.sendText('Elm Pokemon API with Bao.js');
});

app.listen({ port: 3000 });
