import './main.css';
import '../node_modules/select2/dist/css/select2.min.css';

import $ from 'jquery';
import select2 from 'select2';

import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
const root = document.getElementById('root');
let App = Main.embed(root);

App.ports.output.subscribe(options => {
  let $selectContainer = $('#select2-container');

  let select = $('<select>', {
    html: options.map(option => {
      return $('<option>', {
        value: option[0],
        text: option[1]
      });
    })
  }).appendTo($selectContainer);

  let select2 = $(select).select2();

  select2.on('change', event => {
    App.ports.input.send(event.target.value);
  });

  select2.trigger('change');
});

registerServiceWorker();
