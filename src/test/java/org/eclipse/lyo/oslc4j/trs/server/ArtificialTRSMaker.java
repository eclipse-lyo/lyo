/*-
 * Copyright (c) 2016-2017 KTH Royal Institute of Technology.
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
package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class for creating HistoryData objects out of a list of resources. This can be used as a
 * convenience to easily expose all the resources of a server at one point in time using TRS.
 *
 * @version $version-stub$
 * @since 2.3.0
 */
public class ArtificialTRSMaker {

    private final static Logger log = LoggerFactory.getLogger(ArtificialTRSMaker.class);

    /**
     * a boolean flag set to true after the history data is created for the resources once
     */
    private boolean hasBeenTweaked                         = false;
    private int     resourcesWithArtificialChangeDataLimit = -1;

    public ArtificialTRSMaker() {
        super();
    }

    public ArtificialTRSMaker(int resourcesWithArtificialChangeDataLimit) {
        super();
        this.resourcesWithArtificialChangeDataLimit = resourcesWithArtificialChangeDataLimit;
    }

    public List<HistoryData> getPostTweakedHistoryDataForElement(URI elementURI) {
        List<HistoryData> tweakedHd = new ArrayList<>();
        ThreadLocalRandom random = ThreadLocalRandom.current();

        Date now = new Date();
        int modifications = random.nextInt(0, 11);

        while (modifications > 0) {
            modifications--;
            Date modifDate = new Date();
            modifDate.setTime(now.getTime() + 5000);
            now = modifDate;
            HistoryData hd = HistoryData.getInstance(modifDate, elementURI, HistoryData.MODIFIED);
            tweakedHd.add(hd);
        }

        return tweakedHd;
    }

    public void tweakHistories(List<HistoryData> histories, List<AbstractResource> resources) {
        log.info("starting to create artificial change data");
        if (!hasBeenTweaked) {
            log.info("starting to create artificial change data");

            int resourcesWithArtificialChangeData = 1;
            for (AbstractResource simulinkRes : resources) {
                if (resourcesWithArtificialChangeData == resourcesWithArtificialChangeDataLimit) {
                    break;
                }
                histories.addAll(getTweakedHistoryDataForElement(simulinkRes));
                resourcesWithArtificialChangeData++;
            }
            hasBeenTweaked = true;
            log.info("finished creating artificial change data");
        } else {
            log.info("artificial change data already in place. Nothing will be done here.");
        }
    }

    private List<HistoryData> getTweakedHistoryDataForElement(AbstractResource res) {
        List<HistoryData> tweakedHd = new ArrayList<>();

        ThreadLocalRandom random = ThreadLocalRandom.current();

        Date now = new Date();
        HistoryData hd = HistoryData.getInstance(now, res.getAbout(), HistoryData.CREATED);
        tweakedHd.add(hd);

        int modifications = random.nextInt(0, 11);
        while (modifications > 0) {
            modifications--;
            Date modifDate = new Date();
            modifDate.setTime(now.getTime() + 5000);
            now = modifDate;
            hd = HistoryData.getInstance(modifDate, res.getAbout(), HistoryData.MODIFIED);
            tweakedHd.add(hd);
        }
        return tweakedHd;
    }

}
