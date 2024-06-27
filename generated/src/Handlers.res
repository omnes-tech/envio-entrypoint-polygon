type functionRegister = Loader | Handler

let mapFunctionRegisterName = (functionRegister: functionRegister) => {
  switch functionRegister {
  | Loader => "Loader"
  | Handler => "Handler"
  }
}

// This set makes sure that the warning doesn't print for every event of a type, but rather only prints the first time.
let hasPrintedWarning = Set.make()

@genType
type handlerArgs<'event, 'context> = {
  event: Types.eventLog<'event>,
  context: 'context,
}

@genType
type handlerFunction<'eventArgs, 'context, 'returned> = handlerArgs<
  'eventArgs,
  'context,
> => 'returned

@genType
type handlerWithContextGetter<
  'eventArgs,
  'context,
  'returned,
  'loaderContext,
  'handlerContextSync,
  'handlerContextAsync,
> = {
  handler: handlerFunction<'eventArgs, 'context, 'returned>,
  contextGetter: Context.genericContextCreatorFunctions<
    'loaderContext,
    'handlerContextSync,
    'handlerContextAsync,
  > => 'context,
}

@genType
type handlerWithContextGetterSyncAsync<
  'eventArgs,
  'loaderContext,
  'handlerContextSync,
  'handlerContextAsync,
> = SyncAsync.t<
  handlerWithContextGetter<
    'eventArgs,
    'handlerContextSync,
    unit,
    'loaderContext,
    'handlerContextSync,
    'handlerContextAsync,
  >,
  handlerWithContextGetter<
    'eventArgs,
    'handlerContextAsync,
    promise<unit>,
    'loaderContext,
    'handlerContextSync,
    'handlerContextAsync,
  >,
>

@genType
type loader<'eventArgs, 'loaderContext> = handlerArgs<'eventArgs, 'loaderContext> => unit

let getDefaultLoaderHandler: (
  ~functionRegister: functionRegister,
  ~eventName: string,
  handlerArgs<'eventArgs, 'loaderContext>,
) => unit = (~functionRegister, ~eventName, _loaderArgs) => {
  let functionName = mapFunctionRegisterName(functionRegister)

  // Here we use this key to prevent flooding the users terminal with
  let repeatKey = `${eventName}-${functionName}`
  if !(hasPrintedWarning->Set.has(repeatKey)) {
    // Here are docs on the 'terminal hyperlink' formatting that I use to link to the docs: https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
    Logging.warn(
      `Skipped ${eventName} in the ${functionName}, as there is no ${functionName} registered. You need to implement a ${eventName}${functionName} method in your handler file or ignore this warning if you don't intend to implement it. Here are our docs on this topic: \n\n https://docs.envio.dev/docs/event-handlers`,
    )
    let _ = hasPrintedWarning->Set.add(repeatKey)
  }
}

let getDefaultLoaderHandlerWithContextGetter = (~functionRegister, ~eventName) => SyncAsync.Sync({
  handler: getDefaultLoaderHandler(~functionRegister, ~eventName),
  contextGetter: ctx => ctx.getHandlerContextSync(),
})

module EntryPointContract = {
  module AccountDeployed = {
    open Types.EntryPointContract.AccountDeployedEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let accountDeployedLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let accountDeployedHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      accountDeployedLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      accountDeployedHandler :=
        Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      accountDeployedHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      accountDeployedLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="AccountDeployed", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch accountDeployedHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="AccountDeployed",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module BeforeExecution = {
    open Types.EntryPointContract.BeforeExecutionEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let beforeExecutionLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let beforeExecutionHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      beforeExecutionLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      beforeExecutionHandler :=
        Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      beforeExecutionHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      beforeExecutionLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="BeforeExecution", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch beforeExecutionHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="BeforeExecution",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module Deposited = {
    open Types.EntryPointContract.DepositedEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let depositedLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let depositedHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      depositedLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      depositedHandler := Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      depositedHandler := Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      depositedLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="Deposited", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch depositedHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(~eventName="Deposited", ~functionRegister=Handler)
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module SignatureAggregatorChanged = {
    open Types.EntryPointContract.SignatureAggregatorChangedEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let signatureAggregatorChangedLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(
        None,
      )
      let signatureAggregatorChangedHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      signatureAggregatorChangedLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      signatureAggregatorChangedHandler :=
        Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      signatureAggregatorChangedHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      signatureAggregatorChangedLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="SignatureAggregatorChanged", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch signatureAggregatorChangedHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="SignatureAggregatorChanged",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module StakeLocked = {
    open Types.EntryPointContract.StakeLockedEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let stakeLockedLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let stakeLockedHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      stakeLockedLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      stakeLockedHandler := Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      stakeLockedHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      stakeLockedLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="StakeLocked", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch stakeLockedHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="StakeLocked",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module StakeUnlocked = {
    open Types.EntryPointContract.StakeUnlockedEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let stakeUnlockedLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let stakeUnlockedHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      stakeUnlockedLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      stakeUnlockedHandler :=
        Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      stakeUnlockedHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      stakeUnlockedLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="StakeUnlocked", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch stakeUnlockedHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="StakeUnlocked",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module StakeWithdrawn = {
    open Types.EntryPointContract.StakeWithdrawnEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let stakeWithdrawnLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let stakeWithdrawnHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      stakeWithdrawnLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      stakeWithdrawnHandler :=
        Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      stakeWithdrawnHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      stakeWithdrawnLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="StakeWithdrawn", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch stakeWithdrawnHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="StakeWithdrawn",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module UserOperationEvent = {
    open Types.EntryPointContract.UserOperationEventEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let userOperationEventLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let userOperationEventHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      userOperationEventLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      userOperationEventHandler :=
        Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      userOperationEventHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      userOperationEventLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="UserOperationEvent", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch userOperationEventHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="UserOperationEvent",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module UserOperationRevertReason = {
    open Types.EntryPointContract.UserOperationRevertReasonEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let userOperationRevertReasonLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let userOperationRevertReasonHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      userOperationRevertReasonLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      userOperationRevertReasonHandler :=
        Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      userOperationRevertReasonHandler :=
        Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      userOperationRevertReasonLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="UserOperationRevertReason", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch userOperationRevertReasonHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(
          ~eventName="UserOperationRevertReason",
          ~functionRegister=Handler,
        )
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
  module Withdrawn = {
    open Types.EntryPointContract.WithdrawnEvent

    type handlerWithContextGetter = handlerWithContextGetterSyncAsync<
      eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    %%private(
      let withdrawnLoader: ref<option<loader<eventArgs, loaderContext>>> = ref(None)
      let withdrawnHandler: ref<option<handlerWithContextGetter>> = ref(None)
    )

    @genType
    let loader = loader => {
      withdrawnLoader := Some(loader)
    }

    @genType
    let handler = handler => {
      withdrawnHandler := Some(Sync({handler, contextGetter: ctx => ctx.getHandlerContextSync()}))
    }

    // Silence the "this statement never returns (or has an unsound type.)" warning in the case that the user hasn't specified `isAsync` in their config file yet.
    @warning("-21") @genType
    let handlerAsync = handler => {
      Js.Exn.raiseError("Please add 'isAsync: true' to your config.yaml file to enable Async Mode.")

      withdrawnHandler := Some(Async({handler, contextGetter: ctx => ctx.getHandlerContextAsync()}))
    }

    let getLoader = () =>
      withdrawnLoader.contents->Belt.Option.getWithDefault(
        getDefaultLoaderHandler(~eventName="Withdrawn", ~functionRegister=Loader),
      )

    let getHandler = () =>
      switch withdrawnHandler.contents {
      | Some(handler) => handler
      | None =>
        getDefaultLoaderHandlerWithContextGetter(~eventName="Withdrawn", ~functionRegister=Handler)
      }

    let handlerIsAsync = () => getHandler()->SyncAsync.isAsync
  }
}
