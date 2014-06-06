-module(pw_auth).
-export([hash_password/1, check_password/3]).

%% TOOD should make these configurable in app.config
-define(SALT_LENGTH, 16).
-define(HASH_ITERATIONS, 4096).
%% TODO this should call a default_hash_func() function to get default based on erlang version
-define(HASH_FUNCTION, sha).
-define(AUTH_NAME, pbkdf2).

%% @doc Hash a plaintext password, returning hashed password and algorithm details
-spec hash_password(BinaryPass::binary()) ->
			   {pbkdf2, HexPass:: binary() | pbkdf2:hex_list(), binary()}.
hash_password(BinaryPass) when is_binary(BinaryPass) ->
    % TODO: Do something more with the salt?
    % Generate salt the simple way
    Salt = crypto:rand_bytes(?SALT_LENGTH),

    % Hash the original password and store as hex
    {ok, HashedPass} = pbkdf2:pbkdf2(?HASH_FUNCTION, BinaryPass, Salt, ?HASH_ITERATIONS),
    HexPass = pbkdf2:to_hex(HashedPass),
    {pbkdf2, HexPass, Salt}.


%% @doc Check a plaintext password with a hashed password
-spec check_password(BinaryPass::binary(), HashedPassword:: binary() | pbkdf2:hex_list(), Salt::binary()) -> 
			    boolean().
check_password(BinaryPass, HashedPassword, Salt) when is_binary(BinaryPass) ->

    % Hash EnteredPassword to compare to HashedPassword
    {ok, HashedPass} = pbkdf2:pbkdf2(?HASH_FUNCTION, BinaryPass, Salt, ?HASH_ITERATIONS),
    HexPass = pbkdf2:to_hex(HashedPass),
    pbkdf2:compare_secure(HexPass, HashedPassword).
