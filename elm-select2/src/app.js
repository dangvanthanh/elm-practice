import('./Main.elm').then(({ Elm }) => {
  const app = document.getElementById('app');
  Elm.Main.init({ node: app });
});
