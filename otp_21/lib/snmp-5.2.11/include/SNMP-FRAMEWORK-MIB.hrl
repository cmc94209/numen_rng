%%% This file was automatically generated by snmpc_mib_to_hrl version 5.2.11
%%% Date: 19-Jun-2018::09:14:11
-ifndef('SNMP-FRAMEWORK-MIB').
-define('SNMP-FRAMEWORK-MIB', true).

%% Oids

-define(snmpFrameworkMIB, [1,3,6,1,6,3,10]).

-define(snmpFrameworkAdmin, [1,3,6,1,6,3,10,1]).

-define(snmpAuthProtocols, [1,3,6,1,6,3,10,1,1]).

-define(snmpPrivProtocols, [1,3,6,1,6,3,10,1,2]).

-define(snmpFrameworkMIBObjects, [1,3,6,1,6,3,10,2]).

-define(snmpEngine, [1,3,6,1,6,3,10,2,1]).
-define(snmpEngineID, [1,3,6,1,6,3,10,2,1,1]).
-define(snmpEngineID_instance, [1,3,6,1,6,3,10,2,1,1,0]).
-define(snmpEngineBoots, [1,3,6,1,6,3,10,2,1,2]).
-define(snmpEngineBoots_instance, [1,3,6,1,6,3,10,2,1,2,0]).
-define(snmpEngineTime, [1,3,6,1,6,3,10,2,1,3]).
-define(snmpEngineTime_instance, [1,3,6,1,6,3,10,2,1,3,0]).
-define(snmpEngineMaxMessageSize, [1,3,6,1,6,3,10,2,1,4]).
-define(snmpEngineMaxMessageSize_instance, [1,3,6,1,6,3,10,2,1,4,0]).

-define(snmpFrameworkMIBConformance, [1,3,6,1,6,3,10,3]).

-define(snmpFrameworkMIBCompliances, [1,3,6,1,6,3,10,3,1]).

-define(snmpFrameworkMIBGroups, [1,3,6,1,6,3,10,3,2]).

-define(snmpEngineGroup, [1,3,6,1,6,3,10,3,2,1]).


%% Range values
-define(low_snmpEngineID, 5).
-define(high_snmpEngineID, 32).
-define(low_snmpEngineBoots, 1).
-define(high_snmpEngineBoots, 2147483647).
-define(low_snmpEngineTime, 0).
-define(high_snmpEngineTime, 2147483647).
-define(low_snmpEngineMaxMessageSize, 484).
-define(high_snmpEngineMaxMessageSize, 2147483647).


%% Definitions from 'SnmpSecurityLevel'
-define('SnmpSecurityLevel_authPriv', 3).
-define('SnmpSecurityLevel_authNoPriv', 2).
-define('SnmpSecurityLevel_noAuthNoPriv', 1).

%% Default values
-define(default_snmpEngineID, []).
-define(default_snmpEngineBoots, 1).
-define(default_snmpEngineTime, 0).
-define(default_snmpEngineMaxMessageSize, 484).

-endif.
