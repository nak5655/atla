import {
    Disposable,
    ShellExecution,
    Task,
    TaskGroup,
    TaskProvider,
    tasks,
    WorkspaceFolder,
} from 'vscode';
import { Config } from './config';

const TASK_SOURCE = 'Atla';
const TASK_TYPE = 'atla';

export interface Execution {
    command: string;
    args: string[];
    env?: { [key: string]: string };
    // NOTE: Not actually sent by RLS but unifies a common execution definition
    cwd?: string;
}

export function activateTaskProvider(target: WorkspaceFolder, config: Config): Disposable {
    const provider: TaskProvider = {
        provideTasks: () => detectBuildTasks(target, config),
        resolveTask: () => undefined, // unused
    };
    return tasks.registerTaskProvider(TASK_TYPE, provider);
}

function detectBuildTasks(target: WorkspaceFolder, config: Config): Task[] {
    const atlaPath = config.buildSystemPath;

    return [
        { subcommand: 'build', group: TaskGroup.Build },
        //{ subcommand: 'check', group: TaskGroup.Build },
        //{ subcommand: 'test', group: TaskGroup.Test },
        { subcommand: 'clean', group: TaskGroup.Clean },
        { subcommand: 'run', group: undefined },
    ]
        .map(({ subcommand, group }) => ({
            definition: { subcommand, type: TASK_TYPE },
            label: `atla ${subcommand} - ${target.name}`,
            execution: createShellExecution({
                command: atlaPath,
                args: [subcommand],
                cwd: target.uri.fsPath,
            }),
            group,
            problemMatchers: ['$atla'],
        }))
        .map((task) => {
            // NOTE: It's important to solely use the VSCode-provided constructor (and
            // *not* use object spread operator!) - otherwise the task will not be picked
            // up by VSCode.
            const vscodeTask = new Task(
                task.definition,
                target,
                task.label,
                TASK_SOURCE,
                task.execution,
                task.problemMatchers,
            );
            vscodeTask.group = task.group;
            return vscodeTask;
        });
}

function createShellExecution(execution: Execution): ShellExecution {
    const { command, args, cwd, env } = execution;
    const cmdLine = `${command} ${args.join(' ')}`;
    return new ShellExecution(cmdLine, { cwd, env });
}
