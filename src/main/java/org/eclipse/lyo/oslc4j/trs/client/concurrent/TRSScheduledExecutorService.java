/**
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.client.concurrent;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import org.apache.log4j.Logger;

/**
 * A specialization of the ScheduledThreadPoolExecutor propagating the
 * exceptions of the TRS providers outside of their respective threads to the
 * TRS Client class
 *
 * @author Omar
 *
 */
public class TRSScheduledExecutorService extends ScheduledThreadPoolExecutor {

    final static Logger logger = Logger.getLogger(TRSScheduledExecutorService.class);

    public TRSScheduledExecutorService(int corePoolSize) {
        super(corePoolSize);
        // TODO Auto-generated constructor stub
    }

    public TRSScheduledExecutorService(int corePoolSize, ThreadFactory tf) {
        super(corePoolSize, tf);
    }

    @Override
    protected void afterExecute(Runnable r, Throwable t) {
        super.afterExecute(r, t);
        if (t == null && r instanceof Future<?>) {
            try {
                Future<?> future = (Future<?>) r;
                if (future.isDone()) {
                    future.get();
                }
            } catch (CancellationException ce) {
                t = ce;
            } catch (ExecutionException ee) {
                t = ee.getCause();
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt(); // ignore/reset
            }
        }
        if (t != null) {
            logger.error(t);
        }
    }
}
