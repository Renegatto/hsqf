_this addEventHandler ["fired",{ params ["_unit"]; _unit setVehicleAmmo 1; hint (str _unit) }]

// Give inf ammo to artillery group in zeus
private _reg = { 
  private _arty = vehicle _this; 
  private _reload = { params ["_unit"]; _unit setvehicleammo 1}; 
  _arty addEventHandler ["fired",_reload]; 
}; 
{_x call _reg} forEach (units _this);

// Give inf ammo to artillery group in editor
private _reg = { 
  private _arty = vehicle _this; 
  private _reload = { params ["_unit"]; _unit setvehicleammo 1}; 
  _arty addEventHandler ["fired",_reload]; 
}; 
{_x call _reg} forEach (units this);

// Give inf ammo to single arty in zeus
_this addEventHandler ["fired", { (_this # 0) setvehicleammo 1} ]; 

// Give inf ammo to single arty in editor
this addEventHandler ["fired", { (_this # 0) setvehicleammo 1} ]; 

_this addAction ["Arsenal", {["Open",true] spawn BIS_fnc_arsenal}]


// Main
setViewDistance 6000;
myZeus addCuratorEditableObjects [(allUnits + vehicles) select { isPlayer _x }, true];