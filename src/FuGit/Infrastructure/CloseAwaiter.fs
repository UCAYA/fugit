module CloseAwaiter

open System.Threading
open System.Threading.Tasks


let private taskCompletionSource = TaskCompletionSource<bool>()
let private taskCancellationSource = new CancellationTokenSource()

let private delayAsyncs = new ResizeArray<Async<unit>>()
let private delayAsyncsLock = new obj ()

let tryStartDelayTask (f: unit -> Async<unit>) =
    let isCancellationRequested =
        lock delayAsyncsLock <| fun () -> taskCancellationSource.IsCancellationRequested

    if isCancellationRequested then
        false
    else
        let delayTask = f ()

        lock delayAsyncsLock <| fun () -> delayAsyncs.Add delayTask

        async {
            do! delayTask
            lock delayAsyncsLock <| fun () -> delayAsyncs.Remove delayTask |> ignore

            if taskCancellationSource.IsCancellationRequested && delayAsyncs.Count = 0 then
                taskCompletionSource.SetResult true

        }
        |> Async.StartImmediate
        |> ignore

        true


let close () =
    let isCancellationRequested =
        lock delayAsyncsLock <| fun () -> taskCancellationSource.IsCancellationRequested

    if isCancellationRequested then
        None
    else
        task {
            lock delayAsyncsLock
            <| fun () ->
                taskCancellationSource.Cancel()

                if delayAsyncs.Count = 0 then
                    taskCompletionSource.SetResult true

            let! _ = taskCompletionSource.Task
            ()
        }
        |> Some