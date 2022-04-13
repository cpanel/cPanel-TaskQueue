# NAME

cPanel::TaskQueue - FIFO queue of tasks to perform

# SYNOPSIS

```perl
use cPanel::TaskQueue ();

my $queue = cPanel::TaskQueue->new( { name => 'tasks', state_dir => "/home/$user/.cpanel/queue" } );

$queue->queue_task( "init_quota" );
$queue->queue_task( "edit_quota fred 0" );
$queue->queue_tasks( "init_quota", "edit_quota fred 0" );

# Processing loop
while (1) {
    # if work, process, else sleep
    if ( $queue->has_work_to_do() ) {
        eval { $queue->process_next_task() };
        if ( $@ ) {
            Carp::carp( $@ );
        }
    }
    else {
        # wait for work.
        sleep 300;
    }
}
```

# DESCRIPTION

This module provides an abstraction for a FIFO queue of tasks that may be
executed asynchronously. Each command determines whether it runs in the
current process or forks a child to do the work in the background.

The TaskQueue has support for limiting the number of background tasks running
at one time and for preventing duplicate tasks from being scheduled.

# PUBLIC METHODS

- cPanel::TaskQueue->new( $hashref )

    Creates a new TaskQueue object based on the parameters from the supplied hashref.

    - _state\_dir_

        This required parameter specifies a directory where the state file should be
        written.  This directory is created if it does not exist.

    - _cache\_dir_

        _Deprecated_ parameter that has been replaced with _state\_dir_. If no value is
        supplied by _state\_dir_, _cache\_dir_'s value will be used.

    - _name_

        This required parameter specifies the name of the queue. This name is used to
        construct the name of the state file used to store the queue information.

    - _state\_timeout_

        This optional parameter specifies the timeout to use for flocking the state file.
        The value is in seconds and defaults to the cPanel::StateFile default value.

    - _default\_timeout_

        This optional parameter specifies the default amount of time (in seconds) to wait
        for an in-process task to run. The default value is 60 seconds.

        This timeout may be overridden for a particular command by the
        `cPanel::TaskQueue::Processor` object.

    - _max\_timeout_

        This optional parameter specifies the maximum amount of time (in seconds) to wait
        for an in-process task to run. The default value is 300 seconds (5 minutes).

        This value prevents a given `cPanel::TaskQueue::Processor` object from setting the timeout
        too high.

    - _max\_running_

        This optional parameter specifies the maximum number of child processes that the
        queue will allow to be running at one time. If this number is reached, all queue
        processing blocks until one or more child processes exit. The default value is 2.

    - _default\_child\_timeout_

        This optional parameter specifies the default amount of time (in seconds) to wait
        for a child task to run. The default value is 3600 seconds (1 hour).

        This timeout may be overridden for a particular command by the
        `cPanel::TaskQueue::Processor` object.

    If these parameters are not specified, but a `TaskQueue` with this _name_ and
    _state\_dir_ has already been created, the new `TaskQueue` will use the parameters
    that were stored in the file.  This causes all instances of this `TaskQueue`
    to act the same. Providing these parameters also updates all other instances of
    this `TaskQueue` the next time they need to `synch`.

- $q->queue\_task( $command )

    Create a new task from the command and put it at the end of the queue if it meets
    certain minimum criteria.

    - Command must be legal.

        The command type must have been registered with the TaskQueue module.

    - Command must not duplicate a command already in the queue.

        Each command type can have its own definition of duplicate. It can depend on
        one or more of the arguments or not.

    If the task is successfully queued, a non-empty _uuid_ is returned. This id can
    be used to remove the task from the queue at a later time.

    If the task was not queued, a false value is returned.

    The `queue_task` method can also be called with a `cPanel::TaskQueue::Task`
    object which will be tested and inserted as usual.

- $q->queue\_tasks( $command, $command, ... )

    Create new tasks from the commands and put them at the end of the queue if they meet
    certain minimum criteria.  This method will only lock the StateFile once.

    - Commands must be legal.

        The command type must have been registered with the TaskQueue module.

    - Commands must not duplicate a command already in the queue.

        Each command type can have its own definition of duplicate. It can depend on
        one or more of the arguments or not.

    This returns a list of _uuid_s: one for each successfully queued task,
    in the order in which the tasks were given.

    If a task is not queued for whatever reason, the _uuid_ is undef.

    The `queue_tasks` method can also be called with `cPanel::TaskQueue::Task`
    objects which will be tested and inserted as usual.

- $q->unqueue\_task( $uuid )

    Remove the task associated with the supplied _uuid_ from the queue, if it
    has not been processed yet. Returns true on success.

## QUEUE PROCESSING

- $q->has\_work\_to\_do()

    Returns a true value if there are any tasks in the queue and we can process them.
    This method does not block on child processes that are currently running if we
    cannot launch a new task.

- $q->process\_next\_task()

    This method is called to process another task from the wait queue.

    If there are any tasks remaining and we have not reached the limit of tasks we
    can process at once, the next is removed from the queue. The task is checked to
    make certain we know how to process it, if not it is discarded.  Then it is added
    to the processing list and the `cPanel::TaskQueue::Processor` object for that
    command is asked to process it. If we have reached our processing limit, block
    until a task can be executed.

    If the command is completed by the `cPanel::TaskQueue::Processor`, the task is
    removed from the processing list. If the `cPanel::TaskQueue::Processor` launched
    a child process, the task is left in the processing list.

    The method returns true if the task was completed or otherwise removed from the
    system. If the task was launched as a child process, the method returns false. The
    method will also return true if there is nothing to process.

- $q->finish\_all\_processing()

    This method does not return until all tasks currently being processed in the
    background are completed. It is most useful to call as part of shutdown of the
    program that processes the queue. While waiting for processing to complete,
    this method does not start any new tasks out of the queue.

- $q->delete\_all\_unprocessed\_tasks()

    This method deletes all tasks in the deferred or waiting state from the
    `cPanel::TaskQueue`. It does nothing with the Tasks that are currently being
    processed.

    _Warning_: This is probably not a very good idea. However, there may be a
    circumstance where we need to throw away everything that is in the queue and
    this method provides that ability.

## QUEUE INFORMATION

- $q->get\_default\_child\_timeout

    Returns the default timeout value for a child process.

- $q->get\_default\_timeout

    Returns the default timeout value for a task.

- $q->get\_max\_running

    Returns the maximum number of child processes that can be running at once.

- $q->get\_max\_timeout

    Returns the maximum timeout value for a task.

- $q->get\_name

    Returns the TaskQueue's name.

- $q->peek\_next\_task()

    Get a copy of the first task descriptor in the queue or `undef` if the queue is
    empty.

    Because of the nature of a task queue, there is no guarantee that this task will
    remain unscheduled after the method call. That is one reason that a copy is
    returned.

- $q->is\_task\_queued( $uuid )

    Does the specified _uuid_ reference a task in the queue?

    Because of the nature of a task queue, the particular _uuid_ tested may be scheduled
    for processing immediately after the test. Therefore, a true answer is not as useful as
    it might seem. A false answer does tell us that the item is no longer waiting.

- $q->find\_task( $uuid )

    Returns a copy of the task in the queue with the supplied _uuid_. Returns
    `undef` if no task with that _uuid_ is found. Because of the nature of the
    task queue, the task that is returned may not be in the queue shortly after
    return from this method. Another process may have handled it and removed it from
    the queue.

    However, the returned copy is a faithful representation of the task at the point
    in time that it was found.

- $q->find\_command( $command )

    Returns a copy of the first command with the supplied _command_ (sans
    arguments).  Returns `undef` if no task with that command name is found.
    Because of the nature of the task queue, the task that is returned may not be
    in the queue shortly after return from this method. Another process may have
    handled it and removed it from the queue.

    Remember that `$command` is just the name of the command, not the whole
    command string with arguments.

- $q->find\_commands( $command )

    Returns a list of copies of commands with the supplied _command_ (sans
    arguments).  Because of the nature of the task queue, the tasks that are
    returned may not be in the queue shortly after return from this method. Another
    process may have handled one or more tasks and removed then from the queue.

    Remember that `$command` is just the name of the command, not the whole
    command string with arguments.

- $q->how\_many\_queued()

    Gives a count at this particular point in time of the number of items currently
    in the queue. Since an item may be removed and processed any time the
    `process_next_task()` method is called, this count may not be correct immediately
    after the method returns.

    Most useful for the general case of telling if the queue is really full, or mostly
    empty.

- $q->is\_task\_processing( $uuid )

    Does the specified _uuid_ reference a task currently being processed?

    Because of the nature of a task queue, the particular _uuid_ tested may be scheduled
    for processing or finish processing immediately after the test. I'm not sure if this
    test is actually useful for anything.

- $q->is\_task\_deferred( $uuid )

    Does the specified _uuid_ reference a task that is currently deferred?

    Because of the nature of a task queue, the particular _uuid_ tested may be scheduled
    for processing or finish processing immediately after the test. I'm not sure if this
    test is actually useful for anything.

- $q->how\_many\_deferred()

    Returns a count of the number of tasks currently that are currently in the
    deferred state. Since a task can complete at any time, the exact value returned
    by this method is not guaranteed for any length of time after the method
    returns.

- $q->how\_many\_in\_process()

    Returns a count of the number of items currently being processed. Since a task
    can complete at any time, the exact value returned by this method is not
    guaranteed for any length of time after the method returns. May be useful to get
    a statistical measure of how busy the `cPanel::TaskQueue` system is.

- $q->snapshot\_task\_lists()

    Returns a reference to a hash containing copies of the current queues. The value
    of _waiting_ is an array reference containing copies of all of the `Task`s
    waiting to execute. The value of _processing_ is an array reference containing
    copies of all of the `Tasks` currently being processed.

    Since a task can complete at any time and whatever process handles the queue can
    start processing a task at any time, the output of this method may be out of
    date as soon as it returns. This method is only really useful for a general idea
    of the state of the queue.

- $q->pause\_processing()

    Prevent any more tasks from moving from the waiting state into the processing
    state. This does not stop any tasks from processing once they begin processing.
    If the queue is paused, no more tasks will move from the waiting state to the
    processing state.

- $q->resume\_processing()

    Allow the queue to resume processing tasks.

- $q->is\_paused()

    Returns true if the queue processing has been paused, false otherwise.

## CACHE SUPPORT

These methods should not be used directly, they exist to support the `cPanel::StateFile`
interface that persists the queue information to disk.

- $q->load\_from\_cache( $fh )

    This method loads the queue information from the disk state file. It is called
    by the `cPanel::StateFile` object owned by this object.

    The user of this class should never need to call this method.

- $q->save\_to\_cache( $fh )

    This method saves the queue information to the disk state file. It is called by
    the `cPanel::StateFile` object owned by this object.

    The user of this class should never need to call this method.

- $q->throw( $msg )

    Log the supplied message and `die`.

- $q->warn( $msg )

    Log the supplied message as a warning.

- $q->info( $msg )

    Log the supplied message as an informational message.

# CLASS METHODS

The class also supports a few methods that apply to the Task Queuing system as a whole.
These methods manage the registering of task processing objects.

- cPanel::TaskQueue->register\_task\_processor( $cmdname, $processor )

    Add a task processing object for the command name given as the first argument.
    The second argument must either be a `cPanel::TaskQueue::Processor`-derived object
    or a code reference that will be wrapped in a `cPanel::TaskQueue::Processor::CodeRef`
    object.

- cPanel::TaskQueue->unregister\_task\_processor( $cmdname )

    Removes the task processing object for the command given as the only argument.

    After a call to this method, that particular command can not be queued any more
    and any already queued objects will be discarded when the `cPanel::TaskQueue`
    tries to process them.

# LOGGER OBJECT

By default, the `TaskQueue` uses `die` and `warn` for all messages during
runtime. However, it supports a mechanism that allows you to insert a
logging/reporting system in place by providing an object to do the logging for
us.

To provide a different method of logging/reporting, supply an object to do the
logging as follows when `use`ing the module.

```perl
use cPanel::TaskQueue ( '-logger' => $logger );
```

The supplied object should supply (at least) 4 methods: `throw`, `warn`,
`info`, and `notify`. When needed these methods will be called with the
messages to be logged.

This only works once for a given program, so you can't reset the policy in
multiple modules and expect it to work.

In addition to setting a global logger, a new logger object can be supplied
when creating a specific `TaskQueue` object.

See [cPanel::TaskQueue::Cookbook](https://metacpan.org/pod/cPanel%3A%3ATaskQueue%3A%3ACookbook) for examples.

# DIAGNOSTICS

The following messages can be reported by this module:

- `Missing command in register_task_processor.`

    No command name was given when calling the `register_task_processor` class
    method to register a processing object to handle a command.

- `Missing task processor in register_task_processor.`

    No command processor object was supplied when calling the `register_task_processor`
    class method to register an action to attach to a command.

- `Command '%s' already has a TaskQueue::Processor registered.`

    The supplied command name already has a registered processing object. If you want
    to change it, you must first remove the other processor using
    `unregister_task_processor`.

- `Unrecognized task processor object.`

    The second parameter to `register_task_processor` was not recognized as a
    `TaskQueue::Processor`-derived object or as a `coderef`.

- `Missing command in unregister_task_processor.`

    No command name string was supplied when calling this method.

- `Command '%s' not registered, ignoring.`

    The supplied argument to `unregister_task_processor` was not a registered
    command name.

- `No caching directory supplied.`

    The required _state\_dir_ parameter was missing when constructing the `TaskQueue`
    object. The object was not created.

- `No queue name supplied.`

    The required _name_ parameter was missing when constructing the `TaskQueue`
    object. The object was not created.

- `Not a recognized TaskQueue state file.`
- `Invalid version of TaskQueue state file.`

    Either the state file is invalid or it is not a TaskQueue state file.

- `Cannot queue an empty command.`

    The command string supplied to `queue_task` was either `undef` or empty.

- `Task with 0 retries not queued.`

    The `Task` supplied to `queue_task` has a remaining retry count of 0. The task
    has been discarded. This is a warning message only.

- `No known processor for '%s'.`

    The specified command has no defined processor. The command has been discarded.

- `Requested command [%s] has invalid arguments.`

    The supplied full command has arguments that are not valid as defined by the
    command processor.

- `No Task uuid argument passed to %s.`

    The specified method requires a _uuid_ to specify which task to operate
    on. None was supplied.

- `No processor found for '%s'.`

    Either the program inserting tasks into the queue has a different list of commands
    than the program processing the queue, or a TaskQueue::Processor was unregistered
    after this command was queued.

- `Task '%s' timed out during processing.`

    The supplied command timed out while being executed in-process.

- `Undefined tasks found in the queue, removing...`

    Somehow a task item of `undef` has appeared in the queue. This should never
    happen, so if it does, we remove them.

# DEPENDENCIES

YAML::Syck, POSIX

cPanel::TaskQueue::Processor, cPanel::TaskQueue::Task, cPanel::StateFile

# INCOMPATIBILITIES

None reported.

# BUGS AND LIMITATIONS

In spite of the locking that's used to prevent multiple concurrent writes
or reads being combined with writes, it is sometimes possible for a state
file to become corrupt or improperly emptied if many processes are attempting
to update it at the same time. There is most likely still a race condition
that's exposed under heavy load.

# SEE ALSO

cPanel::TaskQueue::Processor, cPanel::TaskQueue::Task, and cPanel::StateFile

# LICENCE AND COPYRIGHT

Copyright (c) 2014, cPanel, Inc. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See [perlartistic](https://metacpan.org/pod/perlartistic).

# DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
