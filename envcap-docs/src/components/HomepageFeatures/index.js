import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Capabilities as Modules',
    description: (
      <>
        ENVCAP treats capabilities as first-class modules, enabling fine-grained control over resources and effects while maintaining type safety.
      </>
    ),
  },
  {
    title: 'Separate Compilation',
    description: (
      <>
        The language supports separate compilation of modules while preserving abstraction boundaries through its capability system.
      </>
    ),
  },
  {
    title: 'First-Class Environments',
    description: (
      <>
        Environments are first-class values in ENVCAP, allowing flexible composition and manipulation of module contexts.
      </>
    ),
  },
];

function Feature({title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

function Contributors() {
  return (
    <div className={styles.contributors}>
      <Heading as="h2">Contributors</Heading>
      <div className={styles.contributorList}>
        <div className={styles.contributor}>
          <strong>Main Author:</strong>
          <a href="https://jam-khan.github.io/" target="_blank" rel="noopener noreferrer">
            Jam Kabeer Ali Khan
          </a>
        </div>
        <div className={styles.contributor}>
          <strong>Advisors:</strong>
          <ul>
            <li>
              <a href="https://i.cs.hku.hk/~bruno/" target="_blank" rel="noopener noreferrer">
                Prof. Bruno C. d. S. Oliveira
              </a>
            </li>
            <li>
              <a href="https://jinhaotan.com/" target="_blank" rel="noopener noreferrer">
                Jinhao Tan
              </a>
            </li>
          </ul>
        </div>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
        <Contributors />
      </div>
    </section>
  );
}