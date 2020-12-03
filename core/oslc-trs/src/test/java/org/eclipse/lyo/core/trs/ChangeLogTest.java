package org.eclipse.lyo.core.trs;

import java.util.List;
import static org.assertj.core.api.Assertions.*;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Created on 2017-06-08
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public class ChangeLogTest {
    private final static Logger log = LoggerFactory.getLogger(ChangeLogTest.class);
    @Test
    public void changeEventListNotNullByDefault() throws Exception {
        ChangeLog changeLog = new ChangeLog();

        final List<ChangeEvent> changeEventList = changeLog.getChange();

        assertThat(changeEventList).isNotNull();
    }

    @Test(expected = IllegalArgumentException.class)
    public void changeEventListCantBeSetToNull() throws Exception {
        ChangeLog changeLog = new ChangeLog();
        log.info("Setting changeLog Change Event list to null");
        changeLog.setChange(null);
    }
}
