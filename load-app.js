import show_error from './show-error.mjs';
async function init_app() {
    const compilation_error = await show_error;
    if(compilation_error) {
        return;
    }
    const app = Elm.NumberOrderGame.init({node: document.body, flags: {}});
    window.app = app;

    app.ports.setCSSVariable.subscribe(([k,v]) => {
        document.body.style.setProperty(`--${k}`,v);
    });
}

init_app();
