private _reg = {
    params call [_arty];
    private _reload = {
        params call [_unit];
        setvehicleAmmo call [_unit, 1.0];
    };
    private _arty2 = _arty;
    addEventHandler call [_arty2, "fired", _reload];
};
forEach call [_reg, units call this];

private _var0 = ({
    (params) call (["var1"]);
    private _var2 = ({
        (params) call (["var3"]);
        (setvehicleAmmo) call (([_var3]) + (([1.0]) + ([])));
    });
    private _var3 = (_var1); (addEventHandler) call (([_var3]) + ((["fired"]) + (([_var2]) + ([]))));
});
(forEach) call (([_var0]) + (([(units) call (this)]) + ([])));